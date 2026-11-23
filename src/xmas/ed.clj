(ns xmas.ed
  (:require [clojure.string :as str]
            [xmas.api :as api]
            [xmas.buf :as buf]
            [xmas.buflist :as buflist]
            [xmas.cmd :as cmd]
            [xmas.complete :as complete]
            [xmas.dired :as dired]
            [xmas.git :as git]
            [xmas.gitcommit :as gitcommit]
            [xmas.gitlog :as gitlog]
            [xmas.gitstatus :as gitstatus]
            [xmas.gap :as gap]
            [xmas.job :as job]
            [xmas.listbuf :as listbuf]
            [xmas.lsp :as lsp]
            [xmas.mode :as mode]
            [xmas.nav :as nav]
            [xmas.persist :as persist]
            [xmas.proc :as proc]
            [xmas.project :as project]
            [xmas.props :as props]
            [xmas.rect :as rect]
            [xmas.session :as session]
            [xmas.term :as t]
            [xmas.text :as text]
            [xmas.view :as view]
            [xmas.window :as win]
            [xmas.log :as log]
            [nrepl.server :as nrepl]
            [xmas.web :as web]
            [xmas.dev :as dev])
  (:gen-class))

;; --- Editor state ---
;; `defonce` so the dev file-watcher's `require :reload` doesn't replace
;; the live atom with a fresh nil one, wiping the running editor state.

(defonce editor (atom nil))

(def cur cmd/cur)
(def update-cur cmd/update-cur)
(def set-point cmd/set-point)
(def edit cmd/edit)
(def msg cmd/msg)
(def msg-error cmd/msg-error)
(def beginning-of-line cmd/beginning-of-line)
(def end-of-line cmd/end-of-line)
(def beginning-of-buffer cmd/beginning-of-buffer)
(def end-of-buffer cmd/end-of-buffer)
(def insert-newline cmd/insert-newline)
(def upcase-region cmd/upcase-region)
(def downcase-region cmd/downcase-region)
(def capitalize-region cmd/capitalize-region)
(def mark-whole-buffer cmd/mark-whole-buffer)
(def exchange-point-and-mark cmd/exchange-point-and-mark)
(def transpose-chars cmd/transpose-chars)
(def transpose-lines cmd/transpose-lines)

(defn- line-move [dir]
  (fn [s]
    (set-point s
      (fn [t p]
        (let [ln  (gap/line-of t p)
              col (text/display-width t (gap/nth-line-start t ln) p)
              nxt (+ ln dir)]
          (cond
            (neg? nxt)                     0
            (>= nxt (gap/line-count t))    (count t)
            :else (text/pos-at-col t (gap/nth-line-start t nxt)
                                     (gap/nth-line-end t nxt) col)))))))

(def ^:private next-line-raw     (line-move 1))
(def ^:private previous-line-raw (line-move -1))

(defn- scroll-by [line-cmd]
  (fn [s] (nth (iterate line-cmd s) (max 1 (- (:rows s 24) 2)))))

(def scroll-down (scroll-by next-line-raw))
(def scroll-up   (scroll-by previous-line-raw))

;; --- Prefix argument (C-u) ---

(defn- consume-arg
  "Return [n s'] where n is the active numeric prefix arg (default `d`) and
   s' has :prefix-arg cleared."
  [s d]
  (let [pa (:prefix-arg s)
        n  (or (:num pa) (:mul pa) d)]
    [n (dissoc s :prefix-arg)]))

(defn universal-argument
  "C-u: start or extend the prefix argument. Each repeat multiplies by 4."
  [s]
  (assoc s :prefix-arg {:mul (* 4 (or (:mul (:prefix-arg s)) 1))}))

(defn- repeat-cmd
  "Apply `f` to `s` `n` times (non-negative)."
  [f s n]
  (nth (iterate f s) (max 0 (long n))))

(defn- prefix-repeat
  "Wrap a (state)→state command so it honors :prefix-arg by running n times."
  [f] (fn [s] (let [[n s] (consume-arg s 1)] (repeat-cmd f s n))))

(defn- prefix-aware
  "Wrap an (s, n)→s command so its 0-arity reads n from :prefix-arg.
   Unlike prefix-repeat, this hands n straight to the underlying command —
   useful for commands that compute the n-step result directly rather
   than iterating a 1-step motion."
  [f]
  (fn ([s]   (let [[n s] (consume-arg s 1)] (f s n)))
      ([s n] (f s n))))

(def forward-char  (prefix-aware cmd/forward-char))
(def backward-char (prefix-aware cmd/backward-char))

(def forward-word         (prefix-repeat cmd/forward-word))
(def backward-word        (prefix-repeat cmd/backward-word))
(def next-line            (prefix-repeat next-line-raw))
(def previous-line        (prefix-repeat previous-line-raw))
(def delete-char          (prefix-repeat cmd/delete-char))
(def delete-backward-char (prefix-repeat cmd/delete-backward-char))

(defn self-insert [s key]
  (if (or (char? key) (string? key))
    (cmd/insert-at-point s (str key))
    s))

(def ^:private kill-ring-limit 60)
(defn- kill-push [s text]
  (update s :kill (fnil buf/bounded-conj []) kill-ring-limit text))

(defn- kill-range
  "Delete [from,to) from the current buffer and push the deleted text onto :kill."
  [s from to]
  (if (< from to)
    (let [t (:text (cur s))]
      (-> (edit s from to "") (kill-push (gap/substr t from to))))
    s))

(defn kill-line [s]
  (let [b (cur s) p (:point b) t (:text b)
        eol (gap/line-end t p)
        end (if (= p eol) (min (inc eol) (count t)) eol)]
    (kill-range s p end)))

(defn kill-region [s]
  (if-let [[lo hi] (cmd/region-bounds s)]
    (-> (kill-range s lo hi)
        (update-cur #(assoc % :mark nil)))
    s))

(defn yank [s]
  (if (seq (:kill s))
    (let [p (:point (cur s))] (edit s p p (peek (:kill s))))
    s))

(defn set-mark [s] (update-cur s #(assoc % :mark (:point %))))

(defn- replay-cmd [src-key buf-fn label]
  (fn [s]
    (if (seq (src-key (cur s)))
      (update-cur s buf-fn)
      (msg s (str "No " label)))))

(def undo (replay-cmd :undo buf/undo "undo"))
(def redo (replay-cmd :redo buf/redo "redo"))

(declare ensure-fallback-buf)

(defn keyboard-quit [s]
  (msg (if (:mini s)
         ;; If the saved prev-buf was killed while the minibuffer was
         ;; open, fall back so :buf never ends up pointing at a missing
         ;; entry.
         (let [[s target] (ensure-fallback-buf s (:prev-buf (:mini s)))]
           (assoc s :buf target :mini nil))
         (update-cur s #(assoc % :mark nil)))
       "Quit"))

(defn goto-line [s input]
  (if-let [n (try (Integer/parseInt (str/trim input)) (catch Exception _ nil))]
    (set-point s (fn [t _]
      (gap/nth-line-start t (-> n dec (max 0) (min (dec (gap/line-count t)))))))
    (msg s "Not a number")))

(defn- auto-save-path
  "#file# backup path for a buffer file."
  [^String file]
  (let [f (java.io.File. file)
        parent (or (.getParent f) ".")]
    (str parent "/#" (.getName f) "#")))

(defn- auto-save-file ^java.io.File [^String file]
  (java.io.File. (auto-save-path file)))

(def ^:private auto-save-threshold 300)

;; --- Shell command (M-!) ---

(defn- run-shell
  "Run `cmd` via /bin/sh -c. If `stdin` is given, pipes it in. Returns combined
   stdout + stderr."
  [^String cmd & [^String stdin]]
  (try
    (let [pb (doto (ProcessBuilder. ["sh" "-c" cmd])
               (.redirectErrorStream true))
          p  (.start pb)]
      (if stdin
        (with-open [os (.getOutputStream p)]
          (.write os (.getBytes stdin "UTF-8")))
        (.close (.getOutputStream p)))
      (let [out (slurp (.getInputStream p) :encoding "UTF-8")]
        (.waitFor p)
        out))
    (catch Exception e (str "Error: " (.getMessage e)))))

(defn shell-command-on-region
  "Pipe the current region through `cmd` and replace it with the command's
   output. Requires an active mark."
  [s cmd]
  (cond
    (str/blank? cmd)         s
    (nil? (:mark (cur s)))   (msg s "No region")
    :else
    (let [[lo hi] (cmd/region-bounds s)
          input   (gap/substr (:text (cur s)) lo hi)
          output  (str/trimr (run-shell cmd input))]
      (-> s (edit lo hi output)
            (update-cur #(assoc % :mark nil))))))

(defn shell-command
  "Execute `cmd` in a subshell. Short output goes to the echo area; long
   output opens *Shell Command Output*."
  [s cmd]
  (if (str/blank? cmd)
    s
    (let [output (str/trimr (run-shell cmd))
          multiline? (.contains output "\n")]
      (if multiline?
        (let [name "*Shell Command Output*"]
          (-> s (assoc-in [:bufs name] (buf/make name output nil))
                (cmd/set-cur-buffer name)))
        (msg s output)))))

(defn async-shell-command
  "M-!: run `cmd` as a job so a slow command never freezes the editor. The
   shell string is user input at this boundary; xmas.proc only sees argv."
  [s ^String cmd]
  (if (str/blank? cmd)
    s
    (job/start s {:argv ["sh" "-c" cmd]
                  :buf-name "*Shell Command*"
                  :dir (some-> (:file (cur s)) (java.io.File.) (.getParent))})))

(defn- auto-save-due
  "Pure: return [[buf-name file text version] ...] for every buffer at or above the
   auto-save threshold."
  [s]
  (keep (fn [[name b]]
          (when (and (:file b) (>= (or (:edit-count b) 0) auto-save-threshold))
            [name (:file b) (str (:text b)) (:version b)]))
        (:bufs s)))

(defn- reset-edit-counts
  "Pure: reset :edit-count to 0 for each buf still at or above threshold.
   Guards against racing edits: a concurrent save-buffer that already
   zeroed the counter is respected."
  [s due]
  (reduce (fn [s [name _ _ version]]
            (cond-> s
              (and (= version (get-in s [:bufs name :version]))
                   (>= (or (get-in s [:bufs name :edit-count]) 0)
                       auto-save-threshold))
              (assoc-in [:bufs name :edit-count] 0)))
          s due))

(defn- spit-due!
  "Write each [_ file text version] tuple to its auto-save path. Returns the
   entries written successfully and logs failures.
   Uses atomic-spit so a crash mid-write can't leave the backup truncated —
   the whole point of the backup is to be usable after unclean shutdown."
  [due]
  (keep (fn [[_ file text :as entry]]
          (try
            (persist/atomic-spit (auto-save-path file) text)
            entry
            (catch Exception e
              (log/log "auto-save failed:" file (.getMessage e))
              nil)))
        due))

(defn auto-save!
  "For each file-backed buffer whose edit-count crossed the threshold,
   write its #name# backup and reset the counter. Returns the updated state.
   Do not call from inside a `swap!` — a CAS retry would re-spit the file."
  [s]
  (let [due (auto-save-due s)]
    (reset-edit-counts s (spit-due! due))))

(defn- remember-auto-saves!
  "Persist the set of files with a live `#file#` backup, so the next start can
   offer recovery. Entries whose backup is gone are pruned."
  [saved]
  (when (seq saved)
    (let [paths (->> (concat (persist/load! "auto-saved" [])
                             (map second saved))
                     (filter string?)
                     distinct
                     (filter #(.exists (auto-save-file %)))
                     vec)]
      (persist/save-quiet! "auto-saved" paths))))

(defn- auto-save-tick!
  "Drive auto-save against the editor atom. Spits happen once per tick
   (outside `swap!`), then the counter reset is swapped in atomically."
  [editor-atom]
  (let [due (auto-save-due @editor-atom)]
    (when (seq due)
      (let [saved (spit-due! due)]
        (when (seq saved)
          (remember-auto-saves! saved)
          (swap! editor-atom reset-edit-counts saved))))))

(defn save-buffer-named
  "Save buffer `name` (a :bufs key). Writes the file and records the saved
   version in the edit journal, so a crash before the next session snapshot
   can't replay edits the file already has."
  [s name]
  (let [b (cmd/buf s name)
        f (:file b)]
    (cond
      (nil? b) (msg s (str "No buffer: " name))
      (str/blank? f) (msg s "No file")
      :else
      (try (let [raw  (str (:text b))
                 ;; Preserve the EOL style the file had on read. Without
                 ;; this, opening and saving a CRLF file silently rewrote
                 ;; it as LF — a destructive edit we didn't intend.
                 bytes (if (= :crlf (:line-ending b))
                         (str/replace raw "\n" "\r\n") raw)]
             (persist/atomic-spit f bytes)
             (session/note-saved! name (:version b)))
           (let [bak (auto-save-file f)]
             (when (.exists bak) (.delete bak)))
           (-> s (update-in [:bufs name] #(assoc % :modified false :edit-count 0))
                 (msg (str "Wrote " f)))
           (catch Exception e (msg-error s "Save failed" e))))))

(defn save-buffer [s] (save-buffer-named s (:buf s)))

(defn- open-buf [s path b]
  (let [s (-> s (assoc-in [:bufs path] b) (cmd/set-cur-buffer path))
        m (mode/mode-for-file path)]
    (cond-> s m (mode/set-major-mode m))))

(defn- file-info [filename]
  (let [f (.getAbsoluteFile (java.io.File. filename))]
    {:path (.getCanonicalPath f) :name (.getName f)
     :exists (.exists f) :dir? (.isDirectory f)}))

(def ^:private recentf-limit 50)

(defn- bump-recentf [s path]
  (let [r (vec (take recentf-limit
                     (cons path (remove #{path} (:recentf s)))))]
    (persist/save-quiet! "recentf" r)
    (assoc s :recentf r)))

(defn find-file [s filename]
  (let [{:keys [path name exists dir?]} (file-info filename)]
    (cond
      ;; already open — just switch to it, don't clobber in-memory edits
      (contains? (:bufs s) path) (-> s (cmd/set-cur-buffer path) (bump-recentf path))

      ;; Opening a directory drops into dired, matching Emacs's C-x C-f.
      dir? (dired/open s path)

      exists
      (try (let [raw (slurp path :encoding "UTF-8")]
             (-> (open-buf s path (-> (buf/make name (buf/normalize-eol raw) path)
                                      (assoc :line-ending (buf/detect-eol raw))))
                 (bump-recentf path)))
           (catch Exception e (msg-error s (str "Error reading " path) e)))

      :else (-> (open-buf s path (buf/make name "" path))
                (bump-recentf path)
                (msg "(New file)")))))

(defn switch-buffer [s name]
  (cond
    (str/blank? name) s
    ;; Reserve internal buffer names (leading space) — letting the user
    ;; switch into " *mini*" would wedge the minibuffer machinery on the
    ;; next prompt.
    (.startsWith ^String name " ") (msg s (str "Reserved buffer name: " name))
    (cmd/buf s name) (cmd/set-cur-buffer s name)
    :else (-> s (assoc-in [:bufs name] (buf/make name)) (cmd/set-cur-buffer name))))

(defn kill-buffer
  "Remove `name` (defaults to current). Refuses to kill the last buffer.
   Windows showing the killed buffer fall back to the next visible buffer
   (or *scratch* if none remain)."
  [s name]
  (let [target (if (str/blank? name) (:buf s) (str/trim name))]
    (cond
      (not (contains? (:bufs s) target)) (msg s (str "No buffer: " target))
      (= 1 (count (:bufs s)))             (msg s "Cannot kill last buffer")
      :else
      (let [file (:file (get-in s [:bufs target]))
            [s' fallback] (ensure-fallback-buf (update s :bufs dissoc target) nil)]
        (when file (lsp/close-doc! file))
        (-> s'
            (update :windows win/replace-buffer target fallback)
            (cond-> (= target (:buf s')) (cmd/set-cur-buffer fallback))
            (msg (str "Killed " target)))))))

(defn recenter
  "Move the current window's scroll so point lands on the middle row."
  [s]
  (let [b (cur s) t (:text b) p (:point b)
        rows (max 1 (- (:rows s 24) 2))
        point-line  (gap/line-of t p)
        target-line (max 0 (- point-line (quot rows 2)))
        new-scroll  (gap/nth-line-start t target-line)]
    (assoc-in s (conj (cmd/cur-window-path s) :scroll) new-scroll)))

(defn comment-region
  "Prepend the buffer's :comment-start (default ';; ') to each line in the
   region. No-op without a mark."
  [s]
  (if-let [[lo hi] (cmd/region-bounds s)]
    (let [t  (:text (cur s))
          first-line (gap/line-of t lo)
          last-line  (max first-line (gap/line-of t (max lo (dec hi))))
          cs       (or (:comment-start (cur s)) ";; ")
          fl-start (gap/nth-line-start t first-line)
          ll-end   (gap/nth-line-end   t last-line)
          slice    (str (gap/substr t fl-start ll-end))
          commented (str/join "\n" (map #(str cs %) (str/split slice #"\n" -1)))]
      (-> (edit s fl-start ll-end commented)
          (update-cur #(assoc % :mark nil))))
    (msg s "No region")))

;; --- Minibuffer ---

(def ^:private mini-buf-name " *mini*")

(defn mini-start
  ([s prompt on-done] (mini-start s prompt on-done nil))
  ([s prompt on-done completer]
   (if (:mini s)
     ;; A minibuffer is already active. Opening a second one would drop the
     ;; outer prompt's `prev-buf` / `on-done` and leave the editor wedged.
     (msg s "Command attempted to use disabled command in minibuffer")
     (-> s
         (assoc-in [:bufs mini-buf-name] (assoc (buf/make mini-buf-name) :journal? false))
         (assoc :mini {:prompt prompt :on-done on-done :prev-buf (:buf s)
                       :history-idx -1 :completer completer})
         (assoc :buf mini-buf-name)))))

(defn- ensure-fallback-buf
  "Return [state target] — pick a buffer to restore to when the saved
   target is gone. Prefers an existing non-hidden buffer, otherwise
   creates *scratch* so :buf never points at a missing entry."
  [s prev-buf]
  (cond
    (contains? (:bufs s) prev-buf) [s prev-buf]
    :else
    (if-let [n (first (remove #(.startsWith ^String % " ") (sort (keys (:bufs s)))))]
      [s n]
      [(assoc-in s [:bufs "*scratch*"] (buf/make "*scratch*")) "*scratch*"])))

(defn mini-accept [s]
  (if-let [{:keys [on-done prev-buf]} (:mini s)]
    (let [input  (str (:text (cur s)))
          [s target] (ensure-fallback-buf s prev-buf)
          appended? (and (seq input) (not= input (peek (:mini-history s))))
          s' (-> s
                 (assoc :buf target :mini nil)
                 (cond-> appended? (update :mini-history conj input)))]
      (when appended? (persist/save-quiet! "mini-history" (:mini-history s')))
      (on-done s' input))
    s))

;; --- Crash recovery ---

(defn- pending-recoveries
  "Files with an auto-save backup left by a previous run."
  []
  (->> (persist/load! "auto-saved" [])
       (filter string?)
       distinct
       (filter #(.exists (auto-save-file %)))
       vec))

(defn- recover-file
  "Open `f` and replace its text with the auto-save backup, leaving it marked
   modified so the user can inspect it before C-x C-s."
  [s f]
  (let [backup (auto-save-file f)]
    (if (.exists backup)
      (let [text (slurp backup :encoding "UTF-8")
            s    (find-file s f)]
        (-> (cmd/update-cur s #(assoc % :text (gap/of text) :point 0
                                        :modified true
                                        ;; The backup is different text: cached
                                        ;; spans, props and undo offsets from the
                                        ;; file no longer describe it.
                                        :version (inc (or (:version %) 0))
                                        :props [] :font-lock [] :font-lock-window nil
                                        :undo [] :redo [] :journal []))
            (msg (str "Recovered " f " from auto-save — C-x C-s to keep"))))
      (msg s (str "No auto-save for " f)))))

(defn- recover-ask
  "Prompt for each pending backup. `y` restores it, `n` deletes it."
  [s paths]
  (if-let [[f & more] (seq paths)]
    (mini-start s (str "Recover unsaved changes to " f "? (y/n) ")
      (fn [s ans]
        (if (= "y" (str/lower-case (str/trim ans)))
          (recover-ask (recover-file s f) more)
          (do (try (.delete (auto-save-file f))
                   (catch Exception _))
              (recover-ask s more)))))
    s))

(defn- mini-set
  "Replace minibuffer contents with entry, point at end."
  [s entry]
  (update-in s [:bufs mini-buf-name] assoc
             :text (gap/of entry) :point (count entry)))

(defn- mini-select-candidate
  "Select candidate `j` and put it in the minibuffer, updating :for so the
   async layer does not restart the request."
  [s j]
  (let [cs (vec (get-in s [:mini :candidates]))]
    (if (and (seq cs) (<= 0 j) (< j (count cs)))
      (let [c (nth cs j)]
        (-> s (assoc-in [:mini :sel] j)
              (assoc-in [:mini :for] c)
              (mini-set c)))
      s)))

(defn- mini-next-candidate [s]
  (let [i (get-in s [:mini :sel] -1)
        n (count (get-in s [:mini :candidates]))]
    (mini-select-candidate s (min (inc i) (dec n)))))

(defn- mini-prev-candidate [s]
  (let [i (get-in s [:mini :sel] (count (get-in s [:mini :candidates])))]
    (mini-select-candidate s (max (dec i) 0))))

(defn- mini-history-move
  "Move minibuffer history selection. dir=+1 goes back (older), -1 forward (newer).
   idx=-1 means cleared (no selection); 0..n-1 picks from newest to oldest."
  [s dir]
  (let [hist (:mini-history s [])
        n    (count hist)
        idx  (get-in s [:mini :history-idx])
        new  (-> (+ idx dir) (max -1) (min (dec n)))]
    (cond
      (= new idx) s
      (neg? new)  (-> s (assoc-in [:mini :history-idx] -1) (mini-set ""))
      :else       (-> s (assoc-in [:mini :history-idx] new)
                        (mini-set (get hist (- (dec n) new)))))))

(defn- mini-history-prev [s] (mini-history-move s 1))
(defn- mini-history-next [s] (mini-history-move s -1))

(defn- longest-common-prefix [xs]
  (reduce (fn [a b]
            (apply str (map first (take-while (fn [[x y]] (= x y))
                                              (map vector a b)))))
          xs))

(defn- complete-prefix
  "Core prefix-completion cascade. Returns {:completed :candidates}. `format-one`
   is applied to a unique match to decorate it (e.g. trailing slash for dirs)."
  ([input candidates] (complete-prefix input candidates identity))
  ([input candidates format-one]
   (let [matches (filterv #(.startsWith ^String % input) candidates)]
     (cond
       (empty? matches)      {:completed input :candidates []}
       (= 1 (count matches)) {:completed (format-one (first matches)) :candidates []}
       :else                 {:completed (longest-common-prefix matches)
                              :candidates matches}))))

(defn- file-completer
  "Tab-complete file paths. Returns {:completed str :candidates [str]}."
  ([input] (file-completer input nil))
  ([input _s]
   (let [f (java.io.File. (if (empty? input) "." input))
         [parent prefix] (if (.isDirectory f)
                           [f ""]
                           [(.getParentFile f) (.getName f)])
         parent (or parent (java.io.File. "."))
         children (try (vec (.list parent)) (catch Exception _ []))
         prefixed (fn [s] (str (.getPath parent) "/" s))
         {:keys [completed candidates]}
         (complete-prefix prefix children
           (fn [m] (let [full (prefixed m)]
                     (if (.isDirectory (java.io.File. full)) (str full "/") full))))]
     (cond
       (and (empty? candidates) (= completed prefix)) {:completed input :candidates []}
       (seq candidates)                               {:completed (prefixed completed) :candidates candidates}
       :else                                          {:completed completed :candidates []}))))

(defn- mini-tab-complete [s]
  (if-let [completer (get-in s [:mini :completer])]
    (let [input (str (:text (cur s)))
          {:keys [completed candidates]} (completer input s)]
      (-> s
          (mini-set completed)
          ;; Stash on :mini itself — :msg isn't rendered while a mini is
          ;; active, so the candidate list used to be invisible.
          (assoc-in [:mini :candidates] (vec candidates))))
    s))

(defn- mini-refresh-candidates
  "Run the completer with the current minibuffer text and stash matches on
   :mini. Called after every keystroke so candidates narrow live as the
   user types — no TAB needed. Map completers are asynchronous: they only
   record the input here, and `complete/tick!` fills the candidates."
  [s]
  (let [completer (and (:mini s) (get-in s [:mini :completer]))
        input (str (:text (cur s)))]
    (cond
      (map? completer) (assoc-in s [:mini :input] input)
      completer (assoc-in s [:mini :candidates]
                          (vec (:candidates (completer input s))))
      :else s)))

(declare handle-key)

;; --- Buffer props ---

(defn- set-prop
  "Replace every prop owned by `owner` in the current buffer with one range."
  [s owner face from to]
  (cmd/update-cur s #(assoc % :props (props/set-owner (:props %) owner from to face {}))))

(defn- clear-prop [s owner]
  (cmd/update-cur s #(assoc % :props (props/remove-owner (:props %) owner))))

;; --- Incremental search ---

(defn- isearch-do
  "Search from current point in :isearch's direction. When :extend? is true
   (pattern just grew or shrank), include the current point as a candidate so
   a match anchored there can grow/shrink; otherwise advance past the current
   match so C-s/C-r finds the next occurrence."
  [s]
  (let [{:keys [pattern direction extend?]} (:isearch s)
        b (cur s) t (:text b) p (:point b)
        tn (count t) pn (count pattern)]
    (if (empty? pattern)
      (clear-prop s :isearch)
      (let [found (if (= direction :forward)
                    (text/search-forward  t pattern (if extend? p (min (+ p pn) tn)))
                    ;; Extend: allow the match starting at p (ending at p+pn)
                    ;; to stay anchored. (inc p) only worked for pn=1 — multi-
                    ;; char patterns were yanked away from the current match.
                    (text/search-backward t pattern (if extend? (+ p pn) p)))]
        (if found
          (-> (set-point s (fn [_ _] found))
              (set-prop :isearch :match found (+ found pn)))
          (msg s (str "Failing I-search: " pattern)))))))

(defn isearch-start [s direction]
  (if (:mini s)
    ;; Isearch and the minibuffer can't sensibly share an editor state —
    ;; isearch would operate on the minibuffer text, and the minibuffer's
    ;; prompt would remain live underneath. Refuse.
    (msg s "Command attempted to use disabled command in minibuffer")
    (-> s (assoc :isearch {:pattern "" :direction direction
                           :origin (:point (cur s))}))))

(defn- isearch-accept [s]
  ;; Clear :msg too — otherwise a "Failing I-search: foo" from the final
  ;; keystroke survives into the normal echo area after RET.
  (-> s (clear-prop :isearch) (dissoc :isearch) (assoc :msg nil)))

(defn- isearch-cancel [s]
  (let [origin (get-in s [:isearch :origin])]
    (-> s (clear-prop :isearch)
          (dissoc :isearch)
          (set-point (fn [_ _] origin))
          (msg "Quit"))))

(defn- isearch-append [s ch]
  (-> s (update :isearch assoc :extend? true)
        (update-in [:isearch :pattern] str ch)
        isearch-do))

(defn- isearch-next [s direction]
  (-> s (update :isearch assoc :direction direction :extend? false)
        isearch-do))

(def ^:private isearch-keys
  {[:ctrl \s]  #(isearch-next % :forward)
   [:ctrl \r]  #(isearch-next % :backward)
   :return    isearch-accept
   [:ctrl \g]  isearch-cancel})

(defn- isearch-backspace [s]
  (let [pat (get-in s [:isearch :pattern])]
    (if (seq pat)
      (-> s (update :isearch assoc :extend? true)
            (assoc-in [:isearch :pattern] (subs pat 0 (dec (count pat))))
            isearch-do)
      (isearch-cancel s))))

(defn- handle-isearch
  "Handle a key during incremental search. Returns updated state."
  [s key]
  (cond
    (isearch-keys key)   ((isearch-keys key) s)
    (= key :backspace)   (isearch-backspace s)
    (and (char? key) (>= (int key) 32)) (isearch-append s (str key))
    (string? key)        (isearch-append s key)
    :else                (-> (isearch-accept s) (handle-key key))))

;; --- Eval expression (M-:) ---

(defn- eval-expression [s input]
  ;; Runs inside `(swap! editor ...)`, so it gets a private atom: swapping the
  ;; live one here would make the outer CAS retry and run the form twice.
  (try
    (let [a (atom s)
          result (binding [api/*state* a
                           *ns* (or (find-ns 'user) *ns*)]
                   (load-string input))]
      (msg @a (pr-str result)))
    (catch Exception e (msg-error s "Eval error" e))))

;; --- Bindings ---

(defn- prompt
  "Return a command that opens a minibuffer with `p` as prompt and `on-done` as handler."
  [p on-done & [completer]]
  (fn [s] (mini-start s p on-done completer)))

;; --- Quit: offer to save each modified buffer ---

(defn- modified-bufs [s]
  (->> (:bufs s)
       (keep (fn [[name b]]
               ;; Hidden buffers (leading space) are editor-owned — the
               ;; minibuffer in particular is always "modified" while you type
               ;; in it, and quitting must not ask to save it.
               (when (and (:modified b)
                          (not (.startsWith ^String name " ")))
                 name)))
       sort))

(defn- quit-finish [s] (assoc s :exit true))

(defn- quit-save-all
  "`!` answer: save every modified file buffer, discard the rest, then quit.
   A failed save cancels the quit rather than losing the buffer."
  [s names]
  (let [s (reduce (fn [s n]
                    (if (:file (cmd/buf s n)) (save-buffer-named s n) s))
                  s names)]
    (if (some #(:modified (cmd/buf s %)) names)
      (msg s "Quit canceled: a buffer could not be saved")
      (quit-finish s))))

(defn- quit-ask
  "Ask about one modified buffer at a time. `y` saves (or discards a buffer
   with no file), `n` skips it, `!` saves the rest; any other answer cancels."
  [s names]
  (if-let [[n & more] (seq names)]
    (let [b (:file (cmd/buf s n)) ]
      (if b
        (mini-start s (str "Save " b "? (y/n/!) ")
          (fn [s ans]
            (case (str/lower-case (str/trim ans))
              "y" (let [s (save-buffer-named s n)]
                    (if (:modified (cmd/buf s n))
                      (msg s (str "Quit canceled: " b " was not saved"))
                      (quit-ask s more)))
              "n" (quit-ask s more)
              "!" (quit-save-all s names)
              (msg s "Quit canceled"))))
        (mini-start s (str "Discard changes to " (:name (cmd/buf s n)) "? (y/n/!) ")
          (fn [s ans]
            (case (str/lower-case (str/trim ans))
              "y" (quit-ask s more)
              "n" (quit-ask s more)
              "!" (quit-save-all s names)
              (msg s "Quit canceled"))))))
    (quit-finish s)))

(defn confirm-quit [s]
  (if-let [names (seq (modified-bufs s))]
    (quit-ask (dissoc s :exit-pending :exit) names)
    (quit-finish s)))

(defn suspend
  "Request suspend. The command loop performs it outside swap! so no state
   lock is held while the process is stopped."
  [s]
  (assoc s :suspend-requested true))

;; --- Window commands ---

(defn- do-split [s dir]
  (let [[tree path] (win/split (:windows s) (:cur-window s) dir)]
    (-> s (assoc :windows tree :cur-window path))))

(defn split-window-below [s] (do-split s :stacked))
(defn split-window-right [s] (do-split s :side-by-side))

(defn other-window [s]
  (let [path (win/next-leaf (:windows s) (:cur-window s))]
    (cmd/switch-window s path)))

(defn delete-window [s]
  (let [s (cmd/save-cur-point s)
        [tree path] (win/delete-window (:windows s) (:cur-window s))]
    (-> s (assoc :windows tree) (cmd/switch-window path))))

(defn delete-other-windows [s]
  (let [s (cmd/save-cur-point s)
        [tree path] (win/only (:windows s) (:cur-window s))]
    (-> s (assoc :windows tree :cur-window path))))

(defn- resize [s dir delta]
  (let [total (case dir :stacked (:rows s) :side-by-side (:cols s))
        tree' (win/adjust-size (:windows s) (:cur-window s) dir total delta)]
    (assoc s :windows tree')))

(defn enlarge-window-horizontally [s] (resize s :side-by-side  1))
(defn shrink-window-horizontally  [s] (resize s :side-by-side -1))
(defn enlarge-window              [s] (resize s :stacked       1))
(defn shrink-window               [s] (resize s :stacked      -1))

;; --- Query replace (M-%) ---

(defn- regex-find
  "Return [start end groups] for the next match of `pattern` at or after `from`, or nil."
  [^CharSequence t ^String pattern ^long from]
  (let [m (re-matcher (re-pattern pattern) t)]
    (when (.find m (int from))
      [(.start m) (.end m)
       (vec (for [i (range (inc (.groupCount m)))]
              (or (.group m (int i)) "")))])))

(defn- expand-replacement
  "Replace \\N in `template` with the corresponding match group."
  [^String template groups]
  (str/replace template #"\\(\d)"
               (fn [[_ d]] (or (get groups (Integer/parseInt d)) ""))))

(defn- qr-msg [s]
  (let [{:keys [from to regex?]} (:query-replace s)]
    (msg s (str (if regex? "Query replacing regexp " "Query replacing ")
                from " with " to ": (y/n/!/q)"))))

(defn- qr-exit [s]
  (let [n (get-in s [:query-replace :count] 0)]
    (-> s (clear-prop :qr)
          (dissoc :query-replace)
          (msg (str "Replaced " n " occurrence(s)")))))

(defn- qr-find-from
  "Return [start end groups] for the first match at or after `pos`, or nil."
  [s ^long pos]
  (let [{:keys [from regex?]} (:query-replace s)
        t (:text (cur s))]
    (when (<= pos (count t))
      (if regex?
        (regex-find t from pos)
        (when-let [start (text/search-forward t from pos)]
          [start (+ start (count from)) nil])))))

(defn- qr-show
  "Move point to the match `found` ([start end groups]), or exit when nil."
  [s found]
  (if-let [[start end groups] found]
    (-> s (cmd/goto-char start)
          (set-prop :qr :match start end)
          (update :query-replace assoc :match-end end :groups groups)
          qr-msg)
    (qr-exit s)))

(defn- qr-advance
  "Skip the current match and show the next one. Search starts past the
   current match's end (past a zero-width match) so `n` always progresses."
  [s]
  (let [{:keys [match-end]} (:query-replace s)
        p (:point (cur s))]
    (qr-show s (qr-find-from s (max (inc (long p)) (long (or match-end 0)))))))

(defn- qr-replace-here [s]
  (let [{:keys [to regex? match-end groups]} (:query-replace s)
        p (long (:point (cur s)))
        replacement (if regex? (expand-replacement to groups) to)
        zero-width? (= p (long match-end))
        s2 (-> s (edit p match-end replacement)
                 (update-in [:query-replace :count] (fnil inc 0)))]
    (if zero-width?
      ;; A zero-width match (e.g. regex `$` or `a*`) would be refound at the
      ;; same spot. Step past it — and exit at EOF — so neither the search
      ;; nor replace-all can loop forever.
      (let [s3 (update-cur s2 #(update % :point inc))
            b  (cur s3)]
        (if (>= (long (:point b)) (count (:text b)))
          (qr-exit s2)
          (qr-show s3 (qr-find-from s3 (:point b)))))
      (qr-show s2 (qr-find-from s2 (:point (cur s2)))))))

(defn- qr-replace-all [s]
  (loop [s (qr-replace-here s)]
    (if (:query-replace s) (recur (qr-replace-here s)) s)))

(defn- query-replace-begin
  "Enter query-replace mode at the first match from point."
  [s from to regex?]
  (if-let [error (when regex?
                   (try (let [_pattern (re-pattern from)] nil)
                        (catch java.util.regex.PatternSyntaxException e e)))]
    (msg s (str "Invalid regexp: " (.getDescription error)))
    (let [s (assoc s :query-replace {:from from :to to :count 0 :regex? regex?})]
      (if-let [found (qr-find-from s (:point (cur s)))]
        (qr-show s found)
        (msg (dissoc s :query-replace) "No match")))))

(defn- qr-prompt-to
  "Having read FROM, prompt for TO and kick off the interactive loop."
  [s input regex?]
  (if (str/blank? input)
    s
    (mini-start s (str "Query replace " (when regex? "regexp ") input " with: ")
      (fn [s to] (query-replace-begin s input to regex?)))))

(defn query-replace        [s input] (qr-prompt-to s input false))
(defn query-replace-regexp [s input] (qr-prompt-to s input true))

(defn query-replace-cmd [s]
  (mini-start s "Query replace: " query-replace))

(defn query-replace-regexp-cmd [s]
  (mini-start s "Query replace regexp: " query-replace-regexp))

(def ^:private qr-keys
  {\y qr-replace-here
   \n qr-advance
   \! qr-replace-all
   \q qr-exit
   :return qr-exit
   :escape qr-exit})

;; --- Keyboard macros ---

(defn start-kbd-macro [s]
  (-> s (assoc :macro-recording []) (msg "Defining kbd macro...")))

(defn end-kbd-macro [s]
  (if-let [rec (:macro-recording s)]
    ;; drop the trailing [[:ctrl \x] \)] that triggered this command
    (let [keys (vec (drop-last 2 rec))]
      (-> s (dissoc :macro-recording)
            (assoc :last-macro keys)
            (msg "Keyboard macro defined")))
    (msg s "Not defining kbd macro")))

(defn- replay-macro
  "Reduce `keys` through handle-key without recording them as themselves —
   otherwise a macro invoked while recording would inline its own keys
   into the outer recording."
  [s keys]
  (let [rec (:macro-recording s)
        s'  (reduce handle-key (dissoc s :macro-recording) keys)]
    (cond-> s' rec (assoc :macro-recording rec))))

(defn call-last-kbd-macro [s]
  (if-let [keys (:last-macro s)]
    (replay-macro s keys)
    (msg s "No kbd macro defined")))

(defn- trim-name [^String s] (str/trim (or s "")))

(defn name-last-kbd-macro
  "Store the last recorded macro under `name` for later replay."
  [s name]
  (let [m (:last-macro s)
        n (trim-name name)]
    (cond
      (nil? m)       (msg s "No macro to name")
      (str/blank? n) (msg s "Empty macro name")
      :else (-> s (assoc-in [:named-macros n] m)
                  (msg (str "Named macro: " n))))))

(defn execute-kbd-macro
  "Replay the macro stored under `name`."
  [s name]
  (if-let [keys (get-in s [:named-macros (trim-name name)])]
    (replay-macro s keys)
    (msg s (str "No macro: " name))))

(defn name-last-kbd-macro-cmd
  "M-x entry point: prompt for a macro name and store it."
  [s]
  (mini-start s "Name macro: " name-last-kbd-macro))

(defn execute-kbd-macro-cmd
  "M-x entry point: prompt for a stored macro name and replay it."
  [s]
  (let [candidates (sort (keys (:named-macros s)))
        completer (fn [input _s] (complete-prefix input candidates))]
    (mini-start s "Macro name: " execute-kbd-macro completer)))

;; --- Help (C-h) ---

(declare bindings)

(defn- resolve-binding [s key]
  (or (mode/lookup-key s key)
      (get (:bindings s) key)
      (get bindings key)))

(defn describe-key
  "C-h k: capture the next keypress and report what it is bound to."
  [s]
  (-> s (assoc :capture-next :describe-key)
        (msg "Describe key: ")))

(defn- name-of-fn
  "Reverse-lookup a command name given its fn value."
  [s f]
  (some (fn [[n {:keys [fn]}]] (when (= fn f) n)) (:commands s)))

(defn- capture-describe-key [s key]
  (let [b (resolve-binding s key)
        desc (cond
               (map? b) "prefix key"
               (fn? b)  (if-let [n (name-of-fn s b)]
                          (str n " -- " (get-in s [:commands n :doc]))
                          "(anonymous)")
               (nil? b) "undefined"
               :else    (pr-str b))]
    (-> s (dissoc :capture-next)
          (msg (str (pr-str key) " runs " desc)))))

(defn- describe-function [s name]
  (let [n  (trim-name name)
        cmd (get-in s [:commands n])]
    (cond
      (str/blank? n) s
      cmd (msg s (str n " -- " (:doc cmd)))
      :else (msg s (str "No function: " n)))))

(defn- describe-variable [s name]
  (let [n (trim-name name)]
    (if-let [v (and (not (str/blank? n))
                    (some #(ns-resolve % (symbol n)) (all-ns)))]
      (msg s (str n " = " (if (bound? v) (pr-str @v) "#<unbound>")
                  (when-let [doc (:doc (meta v))] (str "  --  " doc))))
      (msg s (str "No variable: " n)))))


(defn- dired-find-file-at-point
  "RET in a dired buffer: open the entry under point (file → find-file,
   dir → nested dired)."
  [s]
  (if-let [[kind path] (dired/find-file-at-point s)]
    (case kind
      :dir  (dired/open s path)
      :file (find-file s path))
    s))

(def dired-keymap
  {:return dired-find-file-at-point
   \d      dired/mark-delete
   \u      dired/unmark
   \x      dired/do-delete
   \g      dired/revert})

;; --- Projects, occur, imenu ---

(defn- project-root [s]
  (project/root (or (:file (cur s)) (System/getProperty "user.dir"))))

(defn- project-find-file
  "C-x p f: pick a file anywhere in the project by fuzzy name.
   A prefix argument (C-u) clears the project file cache."
  [s]
  (when (:prefix-arg s) (complete/invalidate-cache!))
  (if-let [root (project-root s)]
    (let [files (project/files root)]
      (mini-start s (str "Find file in " root ": ")
        (fn [s input]
          (let [input (str/trim input)
                rel   (cond
                        (.isFile (java.io.File. root input)) input
                        (= 1 (count (project/fuzzy files input)))
                        (first (project/fuzzy files input))
                        :else nil)]
            (if rel
              (find-file s (str root "/" rel))
              (msg s (str "No such file: " input)))))
        {:sources [complete/project-files]}))
    (msg s "Not in a project")))

(defn- symbol-prefix [^String text p]
  (let [before (gap/substr text 0 p)]
    (or (second (re-find #"[\w\-\*\+!\?\.]*$" before)) "")))

(defn complete-symbol-at-point
  "C-c i: complete the symbol at point asynchronously from buffers and the
   project. The candidate is inserted replacing the prefix."
  [s]
  (let [b (cur s) p (:point b)
        prefix (symbol-prefix (str (:text b)) p)]
    (mini-start s "Complete: "
      (fn [s chosen]
        (let [b (cur s)]
          (if (and (= prefix (gap/substr (:text b) (- p (count prefix)) p))
                   (<= (+ p (count chosen)) (count (:text b))))
            (cmd/edit s (- p (count prefix)) p chosen)
            (msg s "Buffer moved; completion skipped"))))
      {:sources (complete/sources-for s)})))

(defn- git-status
  "C-x g: open the git status buffer for the current project."
  [s]
  (if-let [root (git/repo-root (or (:file (cur s)) (System/getProperty "user.dir")))]
    (gitstatus/open s root)
    (msg s "Not a git repository")))

;; --- LSP commands ---

(def ^:private default-lsp-command ["clojure-lsp"])

(defn- lsp-command [s] (or (:lsp-command s) default-lsp-command))

(defn- char-col
  "0-based UTF-16 character index of point within its line — what LSP wants."
  [s]
  (let [b  (cur s)
        t  (:text b)
        p  (:point b)
        ln (gap/line-of t p)]
    (- p (gap/nth-line-start t ln))))

(defn- lsp-when-ready
  "Run `(f state root-id)` once the project's LSP server is initialized.
   `f` returns the new editor state; it is applied with swap! because the
   initialize handshake completes later, on the editor loop."
  [s f]
  (if-let [root (project-root s)]
    (let [root-id (str root)]
      (lsp/ensure! root-id (lsp-command s) root
                   (fn [_] (swap! editor f root-id))))
    (msg s "Not in a project")))

(defn- show-hover
  "Echo an LSP Hover result in the message bar."
  [result]
  (let [c   (:contents result)
        txt (cond (string? c) c
                  (map? c)   (:value c)
                  (seq c)    (str/join "\n" (map :value c)))
        txt (some-> txt (str/replace #"\s+" " "))]
    (when txt
      (swap! editor assoc :msg (str "LSP: " (subs txt 0 (min 200 (count txt))))))))

(defn- jump-to-definition
  "Move point to an LSP Location / LocationLink, exact line and column."
  [res]
  (let [loc   (if (vector? res) (first res) res)
        uri   (or (:uri loc) (:targetUri loc))
        range (or (:range loc) (:targetRange loc))]
    (when (and uri range)
      (let [line (max 0 (get-in range [:start :line] 0))
            ch   (max 0 (get-in range [:start :character] 0))]
        (swap! editor
               (fn [s]
                 (let [s  (find-file s (lsp/uri->path uri))
                       t  (:text (cur s))
                       ln (min line (dec (gap/line-count t)))]
                   (cmd/goto-char s (+ (gap/nth-line-start t ln) ch)))))))))

(defn- lsp-at-point
  "Ask the project's server something at point and hand `cb` the result."
  [s request cb]
  (if-not (:file (cur s))
    (msg s "Buffer has no file")
    (lsp-when-ready
      s
      (fn [state root-id]
        (request root-id (:file (cur state)) (cmd/line-idx state) (char-col state) cb)
        state))))

(defn lsp-hover [s]      (lsp-at-point s lsp/hover show-hover))
(defn lsp-definition [s] (lsp-at-point s lsp/definition jump-to-definition))

;; Completion is cache + push, never blocking: a cache miss starts one async
;; request and returns the cached hits (usually none); when the reply lands we
;; stash it and nudge the minibuffer to re-run its sources, which now hit the
;; cache. So a slow server only delays the *list*, never a keystroke.
(defonce ^:private comp-cache (atom {}))     ; path -> {:v version :pos {[line ch] labels}}
(defonce ^:private comp-inflight (atom {}))  ; [path version line ch] -> millis

(def ^:private comp-inflight-ttl-ms 5000)

(defn- cached-completion [path version line ch prefix]
  (let [{:keys [v pos]} (get @comp-cache path)]
    (when (and (= v version) (contains? pos [line ch]))
      (->> (get pos [line ch])
           (filter #(str/starts-with? % prefix))
           vec))))

(defn- remember-completion! [path version line ch labels]
  (swap! comp-cache
         (fn [m]
           (if (= version (:v (get m path)))
             (assoc-in m [path :pos [line ch]] labels)
             (assoc m path {:v version :pos {[line ch] labels}})))))

(defn- refresh-completion!
  "Make complete/tick! re-run the active minibuffer's sources so a freshly
   cached LSP reply becomes visible."
  []
  (swap! editor (fn [s] (if (:mini s) (assoc-in s [:mini :for] ::stale) s))))

(defn lsp-completion-source
  "M4 completion source backed by the project's LSP server. Never blocks."
  [prefix ctx]
  (let [state (:state ctx)
        b     (cmd/cur state)
        root  (when (:file b) (project-root state))]
    (when root
      (let [root-id  (str root)
            path     (:file b)
            version  (or (:version b) 0)
            line     (cmd/line-idx state)
            ch       (char-col state)
            key      [path version line ch]
            cached   (cached-completion path version line ch prefix)
            inflight (when-let [t (get @comp-inflight key)]
                       (< (- (System/currentTimeMillis) t) comp-inflight-ttl-ms))]
        (cond
          (not (lsp/server-exists? root-id))
          (do (lsp/ensure! root-id (lsp-command state) root (fn [_])) [])

          (not (lsp/ready? root-id)) []
          (or inflight cached)       cached
          :else
          (do
            (swap! comp-inflight assoc key (System/currentTimeMillis))
            (lsp/completion root-id path line ch
                            (fn [res]
                              (let [items  (cond (map? res)    (:items res)
                                                 (vector? res) res)
                                    labels (vec (keep :label items))]
                                (remember-completion! path version line ch labels)
                                (swap! comp-inflight dissoc key)
                                (refresh-completion!))))
            []))))))

(defn- project-search
  "C-x p g: run rg over the project as a job; RET on a hit opens it."
  [s]
  (if-let [root (project-root s)]
    (mini-start s (str "Search project " root ": ")
      (fn [s pattern]
        (if (str/blank? pattern)
          s
          (let [name (str "*rg " pattern "*")]
            (-> (job/start s {:argv ["rg" "--line-number" "--with-filename" "--no-heading"
                                     "--color" "never" "--smart-case" "--" pattern "."]
                             :dir root
                             :buf-name name
                             :mode :rg-mode})
                (assoc-in [:bufs name :nav-root] root)))))
      (fn [input _s] {:completed input :candidates []}))
    (msg s "Not in a project")))

(defn- line-text [s]
  (let [t  (:text (cur s))
        ln (cmd/line-idx s)]
    (gap/substr t (gap/nth-line-start t ln) (gap/nth-line-end t ln))))

(defn nav-jump
  "RET in an occur, imenu or rg buffer: go to that location."
  [s]
  (if-let [e (listbuf/entry-at-point s)]
    (cond
      (:file e) (-> s (find-file (str (:nav-root (cur s)) "/" (:file e)))
                    (goto-line (str (:line e))))
      (:buf e)  (if (contains? (:bufs s) (:buf e))
                  (-> s (cmd/set-cur-buffer (:buf e)) (goto-line (str (:line e))))
                  (msg s "Source buffer is gone"))
      :else s)
    ;; rg buffers are plain text, not listbufs: parse the line under point.
    (if-let [{:keys [file line]} (nav/rg-line (line-text s))]
      (-> s (find-file (str (:nav-root (cur s)) "/" file))
            (goto-line (str line)))
      s)))

(defn- occur [s input]
  (if (str/blank? input)
    s
    (try
      (let [src     (:buf s)
            entries (nav/occur-entries (:text (cur s)) input)
            name    (str "*Occur " input "*")]
        (-> s (assoc-in [:bufs name] (nav/occur-buffer name src input entries))
              (cmd/set-cur-buffer name)))
      (catch Exception e (msg-error s "Bad regexp" e)))))

(defn- imenu [s]
  (let [b    (cur s)
        syms (nav/imenu-symbols b (str (:text b)))]
    (if (empty? syms)
      (msg s "No symbols")
      (let [name "*Imenu*"]
        (-> s (assoc-in [:bufs name] (nav/imenu-buffer name (:buf s) syms))
              (cmd/set-cur-buffer name))))))

(def ^:private cx-c-map
  {\i complete-symbol-at-point
   \h lsp-hover})

(def ^:private nav-keys {:return nav-jump})

(defn- occur-cmd [s] (mini-start s "Occur: " occur))

(def ^:private rg-keys
  {:return nav-jump
   \g      job/rerun
   [:ctrl \c] {[:ctrl \k] job/kill}})

;; --- Config reload ---

(defn- xmas-path [name]
  (str (System/getProperty "user.home") "/.xmas/" name))

(defn reload-config
  "M-x reload-config: re-run ~/.xmas/init.clj. Like M-:, it evaluates against
   a private atom and hands the result back, so api/bind! can't re-enter the
   command loop's swap!."
  [s]
  (let [path (xmas-path "init.clj")]
    (if-not (.exists (java.io.File. path))
      (msg s (str "No config: " path))
      (try
        (let [a (atom s)]
          (binding [api/*state* a] (load-file path))
          (msg @a (str "Reloaded " path)))
        (catch Exception e (msg-error s "Reload failed" e))))))

(def buflist-keymap
  {:return buflist/switch
   \k      buflist/kill
   \g      buflist/revert})

(def job-keymap
  {[:ctrl \c] {[:ctrl \k] job/kill}
   \g         job/rerun})

(defn diff-open-at-point [s]
  ;; In a diff buffer, file headers look like "diff --git a/path b/path"
  ;; We don't have listbuf entries; just open the file in the diff if on a header.
  (let [b (cmd/cur s)
        t (:text b)
        p (:point b)
        line-start (gap/nth-line-start t (gap/line-of t p))
        line-end (gap/nth-line-end t (gap/line-of t p))
        line (gap/substr t line-start line-end)]
    (when-let [[_ path] (re-find #"diff --git a/(.+) b/" line)]
      (let [dir (git/repo-root (git/current-dir (cmd/cur s)))]
        (-> s (find-file (str dir "/" path))
              (cmd/msg (str "Opened " path)))))))

(def gitdiff-keymap
  {[:ctrl \c] {[:ctrl \c] confirm-quit}
   \g         job/rerun
   :return    diff-open-at-point})

(def gitlog-keymap
  {[:ctrl \c] {[:ctrl \c] confirm-quit}
   \g         gitlog/refresh
   :return    gitlog/show-commit})

(def gitstatus-keymap
  {[:ctrl \c] {[:ctrl \c] confirm-quit}
   \s         gitstatus/stage
   \u         gitstatus/unstage
   \d         gitstatus/diff-at-point
   \c         gitstatus/commit-cmd
   \l         gitstatus/log-cmd
   \g         gitstatus/refresh
   :return    gitstatus/diff-at-point})

;; --- Named-command registry (for M-x + help) ---

(declare bookmark-set bookmark-jump recentf-open)

(defn- cmd-entry [f doc] {:fn f :doc doc})

(defn- builtin-commands []
  {"forward-char"         (cmd-entry forward-char "Move point forward one character.")
   "backward-char"        (cmd-entry backward-char "Move point backward one character.")
   "next-line"            (cmd-entry next-line "Move to the next line.")
   "previous-line"        (cmd-entry previous-line "Move to the previous line.")
   "beginning-of-line"    (cmd-entry beginning-of-line "Move to start of current line.")
   "end-of-line"          (cmd-entry end-of-line "Move to end of current line.")
   "forward-word"         (cmd-entry forward-word "Move forward one word.")
   "backward-word"        (cmd-entry backward-word "Move backward one word.")
   "beginning-of-buffer"  (cmd-entry beginning-of-buffer "Move to start of buffer.")
   "end-of-buffer"        (cmd-entry end-of-buffer "Move to end of buffer.")
   "scroll-down"          (cmd-entry scroll-down "Scroll forward one page.")
   "scroll-up"            (cmd-entry scroll-up "Scroll backward one page.")
   "insert-newline"       (cmd-entry insert-newline "Insert a newline at point.")
   "upcase-region"        (cmd-entry upcase-region "Convert the region to uppercase.")
   "downcase-region"      (cmd-entry downcase-region "Convert the region to lowercase.")
   "capitalize-region"    (cmd-entry capitalize-region "Capitalize words in the region.")
   "mark-whole-buffer"    (cmd-entry mark-whole-buffer "Select the whole buffer.")
   "exchange-point-and-mark" (cmd-entry exchange-point-and-mark "Exchange point and mark.")
   "transpose-chars"      (cmd-entry transpose-chars "Transpose characters around point.")
   "transpose-lines"      (cmd-entry transpose-lines "Transpose this line and the previous line.")
   "comment-region"       (cmd-entry comment-region "Comment each line in the region.")
   "delete-char"          (cmd-entry delete-char "Delete the character after point.")
   "delete-backward-char" (cmd-entry delete-backward-char "Delete the character before point.")
   "kill-line"            (cmd-entry kill-line "Kill from point to end of line.")
   "kill-region"          (cmd-entry kill-region "Kill the region between mark and point.")
   "yank"                 (cmd-entry yank "Reinsert the most recent kill.")
   "set-mark"             (cmd-entry set-mark "Set the mark at point.")
   "undo"                 (cmd-entry undo "Undo the last change.")
   "redo"                 (cmd-entry redo "Redo the last undone change.")
   "keyboard-quit"        (cmd-entry keyboard-quit "Cancel the current input or minibuffer.")
   "save-buffer"          (cmd-entry save-buffer "Save the current buffer to its file.")
   "split-window-below"   (cmd-entry split-window-below "Split the current window into top and bottom.")
   "split-window-right"   (cmd-entry split-window-right "Split the current window into left and right.")
   "other-window"         (cmd-entry other-window "Switch to the next window.")
   "delete-window"        (cmd-entry delete-window "Delete the current window.")
   "delete-other-windows" (cmd-entry delete-other-windows "Delete all windows except the current one.")
   "enlarge-window"              (cmd-entry enlarge-window "Make the current window one row taller.")
   "shrink-window"               (cmd-entry shrink-window "Make the current window one row shorter.")
   "enlarge-window-horizontally" (cmd-entry enlarge-window-horizontally "Make the current window one column wider.")
   "shrink-window-horizontally"  (cmd-entry shrink-window-horizontally "Make the current window one column narrower.")
   "bookmark-set"                (cmd-entry bookmark-set "Save point as a named bookmark.")
   "bookmark-jump"               (cmd-entry bookmark-jump "Jump to a named bookmark.")
   "recentf-open"                (cmd-entry recentf-open "Open a recently visited file.")
   "name-last-kbd-macro"         (cmd-entry name-last-kbd-macro-cmd "Save the last recorded macro under a name.")
   "execute-kbd-macro"           (cmd-entry execute-kbd-macro-cmd "Replay a named keyboard macro.")
   "query-replace"               (cmd-entry query-replace-cmd "Interactively replace literal matches.")
   "query-replace-regexp"        (cmd-entry query-replace-regexp-cmd "Interactively replace regex matches (use \\1 etc. in replacement).")
   "project-find-file"           (cmd-entry project-find-file "Find a file in the current project by fuzzy name.")
   "project-search"              (cmd-entry project-search "Search the current project with ripgrep.")
   "occur"                       (cmd-entry occur-cmd "List lines matching a regex in the current buffer.")
   "imenu"                       (cmd-entry imenu "List definitions in the current buffer.")
   "reload-config"               (cmd-entry reload-config "Re-run ~/.xmas/init.clj.")
   "complete-symbol-at-point"    (cmd-entry complete-symbol-at-point "Complete the symbol at point.")
   "git-status"                  (cmd-entry git-status "Show git status for the current project.")
   "git-commit"                  (cmd-entry gitcommit/commit-at-point "Commit staged changes.")
   "git-diff"                    (cmd-entry diff-open-at-point "Show diff for the entry at point.")
   "git-log"                     (cmd-entry gitlog/show-commit "Show commit log.")})

(defn- register-builtin-commands [s]
  (assoc s :commands (builtin-commands)))

;; --- M-x ---

(defn- command-names [s]
  (sort (keys (:commands s))))

(defn- command-completer
  "Tab-complete command names. Returns {:completed str :candidates [str]}."
  [input s]
  (complete-prefix input (command-names s)))

(defn execute-extended-command
  "Look up and invoke a named command. M-x entry point."
  [s name]
  (if (str/blank? name)
    s
    (let [n (str/trim name)
          cmd (get-in s [:commands n])]
      (if cmd ((:fn cmd) s) (msg s (str "No command: " n))))))

;; --- Registers ---
;; Each register is a value under a single character key:
;;   {:kind :point  :buf b :pos n}        — saved location
;;   {:kind :text   :text "..."}          — saved region
;;   {:kind :window :windows w :cur-window p} — saved window config
;; Lookup uses :capture-next, the same one-key-capture mechanism as C-h k.

(defn- read-char-then [s tag prompt]
  (-> s (assoc :capture-next tag) (msg prompt)))

(defn point-to-register [s] (read-char-then s :reg-point  "Point to register: "))
(defn jump-to-register  [s] (read-char-then s :reg-jump   "Jump to register: "))
(defn copy-to-register  [s] (read-char-then s :reg-copy   "Copy to register: "))
(defn insert-register   [s] (read-char-then s :reg-insert "Insert register: "))
(defn window-configuration-to-register [s]
  (read-char-then s :reg-window "Window configuration to register: "))

(defn- capture-register
  "Resolve `tag` against `key`, applying the corresponding register op."
  [s key tag]
  (let [s  (dissoc s :capture-next)
        ch (when (char? key) key)]
    (if-not ch
      (msg s (str "Not a register: " key))
      (case tag
        :reg-point
        (-> s (assoc-in [:registers ch]
                        {:kind :point :buf (:buf s) :pos (:point (cur s))})
              (msg (str "Point saved in register " ch)))

        :reg-jump
        (let [r (get-in s [:registers ch])]
          (cond
            (nil? r) (msg s (str "Register " ch " is empty"))
            (= :point (:kind r))
            (let [{:keys [buf pos]} r]
              (if (contains? (:bufs s) buf)
                (-> s (cmd/set-cur-buffer buf)
                      (set-point (fn [_ _] (min pos (count (:text (cmd/buf s buf)))))))
                (msg s (str "Buffer gone: " buf))))
            (= :window (:kind r))
            (let [windows (:windows r)
                  path (:cur-window r)
                  valid? (every? #(contains? (:bufs s) (:buffer (get-in windows %)))
                                 (win/leaves windows))]
              (if valid?
                (let [target (get-in windows (conj path :buffer))]
                  (-> s (assoc :windows windows :cur-window path :buf target)
                        cmd/load-window-point))
                (msg s "Window configuration references a killed buffer")))
            :else (msg s (str "Register " ch " is text — use C-x r i"))))

        :reg-copy
        (if-let [[lo hi] (cmd/region-bounds s)]
          (let [txt (str (gap/substr (:text (cur s)) lo hi))]
            (-> s (assoc-in [:registers ch] {:kind :text :text txt})
                  (msg (str "Region copied to register " ch))))
          (msg s "No region"))

        :reg-insert
        (let [r (get-in s [:registers ch])]
          (cond
            (nil? r) (msg s (str "Register " ch " is empty"))
            (= :text (:kind r)) (cmd/insert-at-point s (:text r))
            :else (msg s (str "Register " ch " is not text"))))

        :reg-window
        (-> s (assoc-in [:registers ch]
                        {:kind :window :windows (:windows s) :cur-window (:cur-window s)})
              (msg (str "Window configuration saved in register " ch)))))))

(def ^:private register-tags #{:reg-point :reg-jump :reg-copy :reg-insert :reg-window})

;; --- Bookmarks (named, persisted positions) ---

(defn- bookmark-completer [input s]
  (complete-prefix input (sort (map str (keys (:bookmarks s))))))

(defn- bookmark-set-1 [s name]
  (let [n (str/trim name)]
    (if (str/blank? n)
      s
      (let [bookmarks (assoc (:bookmarks s) n
                             {:buf (:buf s) :pos (:point (cur s))})]
        (persist/save-quiet! "bookmarks" bookmarks)
        (-> s (assoc :bookmarks bookmarks)
              (msg (str "Bookmark " n " saved")))))))

(defn- bookmark-jump-1 [s name]
  (let [n (str/trim name)
        bm (get (:bookmarks s) n)]
    (cond
      (str/blank? n)                         s
      (nil? bm)                              (msg s (str "No bookmark: " n))
      (not (contains? (:bufs s) (:buf bm)))  (msg s (str "Buffer gone: " (:buf bm)))
      :else (let [{:keys [buf pos]} bm
                  s' (cmd/set-cur-buffer s buf)
                  cap (count (:text (cmd/buf s' buf)))]
              (set-point s' (fn [_ _] (min pos cap)))))))

(defn bookmark-set  [s] (mini-start s "Bookmark name: " bookmark-set-1))
(defn bookmark-jump [s] (mini-start s "Jump to bookmark: " bookmark-jump-1 bookmark-completer))

;; --- Recentf ---

(defn- recentf-completer [input s]
  (complete-prefix input (:recentf s)))

(defn recentf-open [s]
  (mini-start s "Recent file: " find-file recentf-completer))

;; --- C-x r prefix map (registers + rectangles + bookmarks) ---

(def ^:private cx-r-map
  {\space point-to-register
   \j     jump-to-register
   \s     copy-to-register
   \i     insert-register
   \w     window-configuration-to-register
   \m     bookmark-set
   \b     bookmark-jump
   \k     rect/kill-rectangle
   \M     rect/copy-rectangle
   \y     rect/yank-rectangle
   \c     rect/clear-rectangle
   \t     (prompt "String rectangle: " rect/string-rectangle)})

(def bindings
  {[:ctrl \f] forward-char     [:ctrl \b] backward-char
   [:ctrl \n] next-line        [:ctrl \p] previous-line
   [:ctrl \a] beginning-of-line [:ctrl \e] end-of-line
   [:meta \f] forward-word     [:meta \b] backward-word
   [:meta \<] beginning-of-buffer [:meta \>] end-of-buffer
   :up previous-line :down next-line :right forward-char :left backward-char
   :home beginning-of-line :end end-of-line
   :page-down scroll-down :page-up scroll-up
   [:ctrl \v] scroll-down [:meta \v] scroll-up
   [:meta \g] (prompt "Goto line: " goto-line)
   [:meta \i] imenu
   [:meta \s] {\o occur-cmd}
   [:meta \:] (prompt "Eval: " eval-expression)
   [:meta \x] (prompt "M-x " execute-extended-command command-completer)
   [:meta \.] lsp-definition
   [:meta \%] (prompt "Query replace: " query-replace)
   [:meta \!] (prompt "Shell command: " async-shell-command)
   [:meta \|] (prompt "Shell command on region: " shell-command-on-region)
   :return insert-newline
   [:ctrl \d] delete-char :backspace delete-backward-char
   [:ctrl \k] kill-line [:ctrl \w] kill-region
   [:ctrl \y] yank [:ctrl \space] set-mark [:ctrl \@] set-mark
   [:ctrl \s] #(isearch-start % :forward)
   [:ctrl \r] #(isearch-start % :backward)
   [:ctrl \/] undo [:meta \/] redo [:ctrl \g] keyboard-quit
   [:ctrl \u] universal-argument
   [:ctrl \t] transpose-chars
   [:ctrl \l] recenter
   [:ctrl \z] suspend
   [:meta \;] comment-region
   [:ctrl \h] {\k describe-key
               \f (prompt "Describe function: " describe-function command-completer)
               \v (prompt "Describe variable: " describe-variable)}
      [:ctrl \c] cx-c-map
[:ctrl \x] {[:ctrl \s] save-buffer
               [:ctrl \f] (prompt "Find file: " find-file file-completer)
               [:ctrl \b] buflist/open
               [:ctrl \c] confirm-quit
               [:ctrl \x] exchange-point-and-mark
               [:ctrl \u] upcase-region
               [:ctrl \l] downcase-region
               [:ctrl \t] transpose-lines
               \b         (prompt "Buffer: " switch-buffer)
               \d         (prompt "Dired: " dired/open file-completer)
               \h         mark-whole-buffer
               \k         (prompt "Kill buffer: " kill-buffer)
               \r         cx-r-map
                \p         {\f project-find-file
                            \g project-search}
               \g         git-status
               \0         delete-window
               \1         delete-other-windows
               \2         split-window-below
               \3         split-window-right
               \o         other-window
               \{         shrink-window-horizontally
               \}         enlarge-window-horizontally
               \^         enlarge-window
               \(         start-kbd-macro
               \)         end-kbd-macro
               \e         call-last-kbd-macro}})

(def ^:private mini-keys
  {:return   mini-accept
   [:meta \p] mini-history-prev
   [:meta \n] mini-history-next
   [:ctrl \n] mini-next-candidate
   [:ctrl \p] mini-prev-candidate
   :tab      mini-tab-complete})

(defn handle-key
  "Pure state transition. Handles prefix keys via :pending state.
   Works identically for terminal and web — one key at a time."
  [s key]
  (let [s (assoc s :msg nil)
        s (cond-> s (:macro-recording s) (update :macro-recording conj key))
        s' (cond
             ;; Bracketed paste is one insert, hence one undo entry.
             (and (vector? key) (= :paste (first key)))
             (cmd/insert-at-point s (buf/normalize-eol (second key)))

             ;; capture-next: consume the key as data, don't dispatch
             (= :describe-key (:capture-next s))
             (capture-describe-key (dissoc s :capture-next) key)

             (register-tags (:capture-next s))
             (capture-register s key (:capture-next s))

             ;; pending prefix — resolve second key
             (:pending s)
             (let [prefix-map (:pending s) s (dissoc s :pending)]
               (if-let [cmd (get prefix-map key)]
                 (cond
                   ;; nested prefix (e.g. C-x r → C-x r SPC)
                   (map? cmd) (assoc s :pending cmd)
                   ;; The command consumes :prefix-arg just like a direct
                   ;; dispatch would; strip it here so `C-u 4 C-x C-s` works.
                   (fn? cmd)  (dissoc (cmd s) :prefix-arg)
                   :else      (msg s (str "prefix " key " not a command")))
                 (msg s (str "prefix " key " undefined"))))

             (:query-replace s)
             (let [h (get qr-keys key)] ((or h qr-exit) s))

             (:isearch s) (handle-isearch s key)

             ;; minibuffer-scoped keys
             (and (:mini s) (mini-keys key)) ((mini-keys key) s)

             :else
             (let [pa (:prefix-arg s)
                   digit? (and pa (char? key) (<= (int \0) (int key) (int \9)))]
               (cond
                 digit?
                 (assoc s :prefix-arg
                        {:num (+ (* 10 (or (:num pa) 0))
                                 (- (int key) (int \0)))})
                 :else
                 (let [binding (resolve-binding s key)
                       s' (cond
                            (map? binding) (assoc s :pending binding)
                            (fn? binding)  (binding s)
                            (char? key)    (if (>= (int key) 32) (self-insert s key) s)
                            (string? key)  (self-insert s key)
                            :else          (msg s (str key " undefined")))]
                   ;; Only drop :prefix-arg when a command actually ran.
                   ;; When we stashed a prefix keymap into :pending, the
                   ;; prefix-arg still belongs to the upcoming command.
                   (cond-> s'
                     (and pa
                          (not (map? binding))
                          (not= binding universal-argument))
                     (dissoc :prefix-arg))))))]
    ;; Refresh live-filter candidates while a minibuffer is active.
    (cond-> s'
      (:mini s') mini-refresh-candidates)))

;; --- Main ---

(defn run-key
  "handle-key with a per-command guard: a command that throws must not bring
   down the editor or lose unsaved buffers."
  [s key]
  (try
    (handle-key s key)
    (catch Exception e
      (log/log "command error:" (.getMessage e))
      (msg-error s "Command error" e))))

(defn command-loop []
  (loop []
    (when-let [[rows cols] (t/apply-pending-resize!)]
      (swap! editor assoc :rows rows :cols cols))
    ;; CAS the scroll writeback against the rendered snapshot so a concurrent
    ;; mutation (nREPL/web) doesn't clobber the other thread's update.
    (let [snapshot @editor
          {:keys [sig cur-scroll cur-hscroll font-lock-updates]}
          (view/render snapshot)
          wp (cmd/cur-window-path snapshot)
          with-cache
          (reduce-kv
            (fn [acc buf-name {:keys [version font-lock font-lock-window]}]
              (if (= version (get-in acc [:bufs buf-name :version]))
                (-> acc
                    (assoc-in [:bufs buf-name :font-lock] font-lock)
                    (assoc-in [:bufs buf-name :font-lock-window] font-lock-window))
                acc))
            (assoc snapshot :render-sig sig)
            (or font-lock-updates {}))
          updated (cond-> with-cache
                    (some? cur-scroll)  (assoc-in (conj wp :scroll) cur-scroll)
                    (some? cur-hscroll) (assoc-in (conj wp :hscroll) cur-hscroll))]
      (compare-and-set! editor snapshot updated))
    (let [key (t/read-key-timeout 100)]
      (when key
        (let [kill-before (peek (:kill @editor))]
          (swap! editor run-key key)
          ;; Clipboard is a boundary side effect, so it stays out of the
          ;; pure commands and out of tests.
          (when (and (seq (:kill @editor))
                     (not= kill-before (peek (:kill @editor))))
            (t/set-clipboard! (peek (:kill @editor))))))
      ;; Persist text edits since the last command, not just on the 5s
      ;; snapshot: this is what makes kill -9 lose at most the command in
      ;; flight. Runs outside swap! because it does IO.
      (session/journal-tick! editor)
      ;; Suspend runs here, outside swap!, so no state lock is held while
      ;; stopped. The flag is cleared first so resume doesn't stop again.
      (when (:suspend-requested @editor)
        (swap! editor dissoc :suspend-requested)
        (t/suspend!))
      ;; Pump on every tick, including idle, so jobs stream while nothing is
      ;; being typed — and only from this thread.
      (swap! editor job/pump!)
      (complete/tick! editor)
      (auto-save-tick! editor)
      (session/save-tick! editor)
      (lsp/tick! editor)
      (when (:exit @editor)
        ;; Clean exit: the final snapshot includes every edit, so the journal
        ;; is only deleted after that write succeeds.
        (when (session/save-session! @editor)
          (session/discard-journal!)))
      (when-not (:exit @editor) (recur)))))

(defn- run-all
  "Run each no-arg fn, logging and continuing on failure."
  [fns]
  (doseq [f fns]
    (try (f) (catch Exception e (log/log "shutdown:" (.getMessage e))))))

(def ^:private scratch-text
  ";; xmas\n;; C-f/b/n/p move, C-x C-f open, C-x C-c quit\n\n")

(defn- register-builtin-modes [s]
  (-> s
      (mode/register :fundamental :major)
      (mode/register :clojure-mode :major)
      (mode/register :emacs-lisp-mode :major)
      (mode/register :dired-mode :major :keymap dired-keymap)
      (mode/register :buflist-mode :major :keymap buflist-keymap)
      (mode/register :job-mode :major :keymap job-keymap)
      (mode/register :nav-mode :major :keymap nav-keys)
      (mode/register :rg-mode :major :keymap rg-keys)
      (mode/register :gitstatus-mode :major :keymap gitstatus-keymap)
      (mode/register :gitdiff-mode :major :keymap gitdiff-keymap)
      (mode/register :gitlog-mode :major :keymap gitlog-keymap)
      (mode/register :gitcommit-mode :major :keymap gitcommit/gitcommit-keymap)))

(defn- initial-state [rows cols]
  (-> {:buf "*scratch*"
       :bufs {"*scratch*" (buf/make "*scratch*" scratch-text nil)}
       :kill [] :msg nil :mini nil :mini-history []
       :windows (win/leaf "*scratch*") :cur-window []
       :rows rows :cols cols
       :completion-sources {:default [complete/buffers complete/buffer-symbols]
                            :clojure-mode [complete/buffer-symbols complete/project-symbols lsp-completion-source]}}
      register-builtin-modes
      register-builtin-commands))

(defn- sanitize-persisted [bookmarks recentf history]
  {:bookmarks (if (map? bookmarks)
                (into {}
                      (filter (fn [[name {:keys [buf pos]}]]
                                (and (string? name) (string? buf)
                                     (nat-int? pos))))
                      bookmarks)
                {})
   :recentf (if (sequential? recentf)
              (vec (take recentf-limit (filter string? recentf)))
              [])
   :mini-history (if (sequential? history) (filterv string? history) [])})

(defn- load-persisted-state!
  "Restore bookmarks, recentf, and mini-history from ~/.xmas/. Missing or
  corrupt files are logged and ignored."
  []
  (swap! editor merge
         (sanitize-persisted (persist/load! "bookmarks")
                             (persist/load! "recentf")
                             (persist/load! "mini-history"))))

(defn- load-init-files! []
  (let [load! (fn [name loader]
                (let [p (xmas-path name)]
                  (when (.exists (java.io.File. p))
                    (try (loader p) (log/log "loaded" name)
                         (catch Exception e (swap! editor msg-error name e))))))]
    (load-persisted-state!)
    (load! "init.clj" #(binding [api/*state* editor] (load-file %)))))

(defn- silence-stdio!
  "Redirect stdout/stderr so nREPL output doesn't corrupt the terminal."
  []
  (let [null (java.io.PrintStream. (java.io.OutputStream/nullOutputStream))]
    (System/setOut null)
    (System/setErr null)))

(defn- start-services! []
  (let [srv {:nrepl (nrepl/start-server :port 7888)
             :web   (web/start! editor 1234 handle-key)
             :dev   (dev/start-watcher!)}]
    (log/log "nrepl:7888 web:1234 dev:watching")
    (spit ".nrepl-port" "7888")
    srv))

(defn- stop-services! [{:keys [nrepl web dev]}]
  (run-all [#(dev/stop-watcher! dev)
            #(web/stop! web editor)
            #(nrepl/stop-server nrepl)]))

(defn -main [& _args]
  (log/init!) (log/log "xmas starting")
  (t/install-shutdown-hook!)
  (proc/install-shutdown-hook!)
  (try
    (t/enter-raw-mode!) (t/enable-bracketed-paste!) (t/cls)
    (t/on-resize!)
    (let [[rows cols] (t/terminal-size)]
      (reset! editor (initial-state rows cols))
      ;; nREPL and any other out-of-band caller can use xmas.api directly.
      (alter-var-root #'api/*state* (constantly editor))
      ;; Load session before init.clj so user config can override
      (when-let [sess (session/load-session!)]
        (let [[restored ok] (session/restore-session @editor sess)]
          (when ok (reset! editor (session/recover-journal! restored)))))
      (load-init-files!)
      (swap! editor recover-ask (pending-recoveries))
      ;; Baseline snapshot: the journal needs a version to replay from even if
      ;; the editor dies before the first periodic save.
      (session/save-session! @editor)
      (silence-stdio!)
      (let [services (start-services!)]
        (try (command-loop)
             (finally (stop-services! services)))))
    (finally
      (run-all [#(log/log "xmas exiting") lsp/shutdown-all! proc/kill-all! log/close! t/teardown!]))))
