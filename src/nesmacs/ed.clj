(ns nesmacs.ed
  (:require [clojure.string :as str]
            [nesmacs.api :as api]
            [nesmacs.buf :as buf]
            [nesmacs.buffer-lifecycle :as lifecycle]
            [nesmacs.buflist :as buflist]
            [nesmacs.cmd :as cmd]
            [nesmacs.complete :as complete]
            [nesmacs.dap :as dap]
            [nesmacs.debug :as debug]
            [nesmacs.dired :as dired]
            [nesmacs.fold :as fold]
            [nesmacs.files :as files]
            [nesmacs.git :as git]
            [nesmacs.gitblame :as gitblame]
            [nesmacs.gitcommit :as gitcommit]
            [nesmacs.githunk :as githunk]
            [nesmacs.gitlog :as gitlog]
            [nesmacs.gitstatus :as gitstatus]
            [nesmacs.gap :as gap]
            [nesmacs.job :as job]
            [nesmacs.keymap :as keymap]
            [nesmacs.lang :as lang]
            [nesmacs.language :as language]
            [nesmacs.listbuf :as listbuf]
            [nesmacs.lsp :as lsp]
            [nesmacs.mini :as mini]
            [nesmacs.mode :as mode]
            [nesmacs.nav :as nav]
            [nesmacs.packagehub :as packagehub]
            [nesmacs.packages :as packages]
            [nesmacs.persist :as persist]
            [nesmacs.proc :as proc]
            [nesmacs.project :as project]
            [nesmacs.rect :as rect]
            [nesmacs.register :as register]
            [nesmacs.search :as search]
            [nesmacs.session :as session]
            [nesmacs.term :as t]
            [nesmacs.terminal :as terminal]
            [nesmacs.terminal.tmux :as tmux]
            [nesmacs.testres :as testres]
            [nesmacs.text :as text]
            [nesmacs.view :as view]
            [nesmacs.window :as win]
            [nesmacs.log :as log]
            [nrepl.server :as nrepl]
            [nesmacs.dev :as dev])
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
  (loop [s s i (max 0 (long n))]
    (if (zero? i) s (recur (f s) (dec i)))))

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

(defn keyboard-quit [s]
  (let [s (dissoc s :pending :prefix-arg :capture-next :escape-count)]
    (cond
      (:isearch s) (search/isearch-cancel s)
      (:query-replace s) (search/handle-replace s :escape)
      (:mini s) (msg (mini/close s) "Quit")
      :else (msg (update-cur s #(assoc % :mark nil)) "Quit"))))

(defn goto-line [s input]
  (if-let [n (try (Integer/parseInt (str/trim input)) (catch Exception _ nil))]
    (cmd/goto-line s n)
    (msg s "Not a number")))

;; --- Shell command (M-!) ---

(defn- run-shell
  "Run `cmd` via /bin/sh -c. If `stdin` is given, pipes it in. Returns combined
   stdout + stderr."
  [^String cmd & [^String stdin]]
  (proc/run ["sh" "-c" cmd] nil {:stdin stdin :merge-stderr? true}))

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
   shell string is user input at this boundary; nesmacs.proc only sees argv."
  [s ^String cmd]
  (if (str/blank? cmd)
    s
    (job/start s {:argv ["sh" "-c" cmd]
                  :buf-name "*Shell Command*"
                  :dir (some-> (:file (cur s)) (java.io.File.) (.getParent))})))

;; --- Terminals (tmux backend) ---

(defn terminal-open
  "M-x terminal: a terminal buffer running $SHELL in the current directory."
  [s]
  (terminal/open s))

(defn agent-cli
  "M-x agent: a terminal buffer running the configured AI CLI (default
   $NESMACS_AGENT, else `claude`). Set it with (api/configure-agent! [\"aider\"])."
  [s]
  (terminal/open s (or (:agent-command s)
                       (some-> (System/getenv "NESMACS_AGENT")
                               (str/trim) (not-empty) (str/split #"\s+"))
                       ["claude"])))

(def find-file files/find-file)
(def save-buffer files/save-buffer)
(def save-buffer-named files/save-buffer-named)
(def auto-save! files/auto-save!)
(def confirm-quit files/confirm-quit)

(defn switch-buffer [s name]
  (cond
    (str/blank? name) s
    ;; Reserve internal buffer names (leading space) — letting the user
    ;; switch into " *mini*" would wedge the minibuffer machinery on the
    ;; next prompt.
    (buf/internal-name? name) (msg s (str "Reserved buffer name: " name))
    :else
    ;; find-file keys buffers by path; the mode line shows the file name, so
    ;; accept either and never create a second empty buffer for a real one.
    (if-let [k (or (when (contains? (:bufs s) name) name)
                   (some (fn [[k b]] (when (= name (:name b)) k)) (:bufs s)))]
      (cmd/set-cur-buffer s k)
      (-> s (assoc-in [:bufs name] (buf/make name)) (cmd/set-cur-buffer name)))))

(defn kill-buffer
  "Remove `name` (defaults to current). Refuses to kill the last buffer.
   Windows showing the killed buffer fall back to the next visible buffer
   (or *scratch* if none remain)."
  [s name]
  (let [target (if (str/blank? name) (:buf s) (str/trim name))]
    (cond
      (not (contains? (:bufs s) target)) (msg s (str "No buffer: " target))
      (= 1 (count (:bufs s)))             (msg s "Cannot kill last buffer")
      :else (lifecycle/kill-buffer s target nil))))

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

(def mini-start mini/start)
(def mini-accept mini/accept)

(declare handle-key)

(def isearch-start search/isearch-start)
(def query-replace search/query-replace)
(def query-replace-regexp search/query-replace-regexp)
(def query-replace-cmd search/query-replace-cmd)
(def query-replace-regexp-cmd search/query-replace-regexp-cmd)

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

;; --- Mouse ---

(defn- window-at
  "Return [path rect] of the window containing screen cell [row col], or nil
   for a divider or the echo area."
  [s row col]
  (some (fn [[path rect]]
          (let [{r :row c :col h :rows w :cols} rect]
            (when (and (<= r row (+ r h -1))
                       (<= c col (+ c w -1)))
              [path rect])))
        (:layout (win/frame-geometry s))))

(defn- mouse-window [s {:keys [row col viewports]}]
  (when-let [[path rect] (window-at s row col)]
    (let [window (get-in (:windows s) path)
          viewport (get viewports path)]
      (when (or (nil? viewports)
                (and (= (:buffer window) (:buffer viewport)) (= rect (:rect viewport))
                     (or (nil? (:version viewport))
                         (= (:version viewport) (:version (cmd/buf s (:buffer window)))))))
        [path rect (or viewport window)]))))

(defn- click-point
  "Buffer position under a click at `row`/`col` inside `rect`, or nil on the
   modeline. Clicks below the last line land at end of buffer, like Emacs."
  [s rect viewport row col]
  (let [body-rows (dec (long (:rows rect)))
        r (- (long row) (long (:row rect)))
        c (- (long col) (long (:col rect)))
        buffer (:buffer viewport)
        b (cmd/buf s buffer)
        content (:text b)
        scroll (:scroll viewport 0)
        hscroll (:hscroll viewport 0)
        gutter (:gutter-width viewport (get-in b [:gutter :width] 0))]
    (when (< r body-rows)
      (let [point-line (gap/line-of content (:point b))
            folds (or (:folds viewport) (fold/effective (fold/collapsed-ranges buffer b) point-line))
            first-visible (view/display-row folds (gap/line-of content scroll))
            line (fold/line-at-display folds (+ first-visible r))
            glyph-col (max 0 (- c (view/text-column gutter hscroll)))]
        (if (> line (dec (gap/line-count content)))
          (count content)
          (let [end (gap/nth-line-end content line)
                left (view/visible-line-start content (gap/nth-line-start content line) end hscroll)]
            (text/pos-at-col content left end glyph-col)))))))

(defn- mouse-click
  "Left press: select the clicked window, then put point under the click.
   Terminal buffers only get focus — their point follows the tmux cursor."
  [s {:keys [button action row col] :as event}]
  (if (and (= 0 button) (= :press action) (not (:mini s)))
    (if-let [[path rect viewport] (mouse-window s event)]
      (let [p (when-not (= :terminal-mode (:mode (cmd/buf s (:buffer viewport))))
                (click-point s rect viewport row col))
            s (-> (cmd/switch-window s path)
                  (dissoc :pending :prefix-arg))]
        (if (some? p)
          (-> s (cmd/goto-char p) (update-cur #(assoc % :mark nil)))
          s))
      s)
    s))

(defn- mouse-scroll [s {:keys [direction] :as event}]
  (if-let [[path rect {:keys [buffer scroll folds]}] (mouse-window s event)]
    (let [wp (into [:windows] path)
          {:keys [text point mode] :as b} (cmd/buf s buffer)
          body-rows (dec (:rows rect))]
      (if (or (<= body-rows 0) (= mode :terminal-mode))
        s
        (let [point-line (gap/line-of text point)
              folds (or folds (fold/effective (fold/collapsed-ranges buffer b) point-line))
              row-at #(view/display-row folds %)
              top (row-at (gap/line-of text (or scroll 0)))
              last-top (max 0 (- (fold/visible-count folds (gap/line-count text)) body-rows))
              new-top (-> (+ top (if (= direction :up) -3 3)) (max 0) (min last-top))
              point-row (row-at point-line)
              new-point-row (-> point-row (max new-top) (min (+ new-top body-rows -1)))
              line-start (fn [r] (gap/nth-line-start text (fold/line-at-display folds r)))]
          (if (= top new-top)
            s
            (cond-> (assoc-in s (conj wp :scroll) (line-start new-top))
              (and (= path (:cur-window s)) (not= point-row new-point-row))
              (assoc-in [:bufs buffer :point] (line-start new-point-row)))))))
    s))

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
        completer (fn [input _s] (mini/complete-prefix input candidates))]
    (mini-start s "Macro name: " execute-kbd-macro completer)))

;; --- Help (C-h) ---

(declare bindings)

(defn- resolve-binding [s key]
  (mode/lookup-key s key (keymap/inherit (get bindings key) (get (:bindings s) key))))

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
  (project/root-for (:file (cur s))))

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

(defn- git-status
  "C-x g: open the git status buffer for the current project."
  [s]
  (if-let [root (git/repo-root (git/current-dir (cur s)))]
    (gitstatus/open s root)
    (msg s "Not a git repository")))

(def complete-symbol-at-point language/complete-symbol-at-point)
(defn lsp-completion-source [prefix context] (language/lsp-completion-source editor prefix context))
(defn apply-workspace-edit! [edit] (language/apply-workspace-edit! editor edit))
(defn lsp-hover [s] (language/lsp-hover editor s))
(defn lsp-definition [s] (language/lsp-definition editor s))
(defn lsp-references [s] (language/lsp-references editor s))
(defn lsp-code-action [s] (language/lsp-code-action editor s))
(defn lsp-signature-help [s] (language/lsp-signature-help editor s))
(defn lsp-document-symbols [s] (language/lsp-document-symbols editor s))
(defn lsp-format [s] (language/lsp-format editor s))
(defn lsp-format-region [s] (language/lsp-format-region editor s))
(defn lsp-rename [s] (language/lsp-rename editor s))

(defn- project-search
  "C-x p g: run rg over the project as a job; RET on a hit opens it."
  [s]
  (if-let [root (project-root s)]
    (mini-start s (str "Search project " root ": ")
      (fn [s pattern]
        (if (str/blank? pattern)
          s
          (let [name (str "*rg " pattern "*")]
            (job/start s {:argv ["rg" "--line-number" "--with-filename" "--no-heading"
                                 "--color" "never" "--smart-case" "--" pattern "."]
                         :dir root
                         :buf-name name
                         :mode :rg-mode
                         :follow-output? false}))))
      (fn [input _s] {:completed input :candidates []}))
    (msg s "Not in a project")))

(def nav-jump nav/jump-at-point)

(defn debug-start [s] (debug/start editor s))
(def debug-breakpoint debug/breakpoint)
(def debug-continue debug/continue)
(def debug-next debug/next-line)
(def debug-step-in debug/step-in)
(def debug-step-out debug/step-out)
(def debug-quit debug/quit)
(def debug-stack debug/stack)
(def debug-variables debug/variables)
(def debug-eval debug/evaluate)

(defn- occur [s input]
  (if (str/blank? input)
    s
    (try
      (let [src     (:buf s)
            entries (nav/occur-entries (:text (cur s)) input)
            name    (str "*Occur " input "*")]
        (listbuf/show s name (nav/occur-buffer name src input entries)))
      (catch Exception e (msg-error s "Bad regexp" e)))))

(defn- imenu [s]
  (let [b    (cur s)
        syms (nav/imenu-symbols b (str (:text b)))]
    (if (empty? syms)
      (msg s "No symbols")
      (let [name "*Imenu*"]
        (listbuf/show s name (nav/imenu-buffer name (:buf s) syms))))))

(declare format-buffer toggle-fold fold-all unfold-all git-blame git-stage-hunk
         git-unstage-hunk)

(def ^:private nav-keys {:return nav-jump})

(defn- occur-cmd [s] (mini-start s "Occur: " occur))

(def ^:private rg-keys
  {:return nav-jump
   \g      job/rerun
   [:ctrl \c] {[:ctrl \k] job/kill}})

;; --- Folding ---

(defn toggle-fold
  "C-c TAB: collapse or expand the form at point."
  [s]
  (let [ranges (fold/ranges (:buf s) (cur s))
        line (cmd/line-idx s)]
    (if (empty? ranges)
      (msg s "Nothing to fold")
      (cmd/update-cur s #(assoc % :collapsed
                                     (fold/toggle ranges (set (:collapsed %)) line))))))

(defn fold-all [s]
  (let [ranges (fold/ranges (:buf s) (cur s))]
    (cmd/update-cur s #(assoc % :collapsed (into #{} (map first) ranges)))))

(defn unfold-all [s]
  (cmd/update-cur s #(assoc % :collapsed #{})))

;; --- M9: git blame, hunk staging, forge, tests ---

(defn git-blame [s] (gitblame/open s))
(defn git-stage-hunk [s] (githunk/open s false))
(defn git-unstage-hunk [s] (githunk/open s true))

(defn run-tests [s]
  (let [root (or (project-root s) (System/getProperty "user.dir"))
        default (or (:test-command s) (testres/default-command root))]
    (mini-start s (str "Test command (blank = " (if (str/blank? default) "none" default) "): ")
      (fn [s input]
        (testres/run s (if (str/blank? input) default input)))
      (fn [_ _] {:completed "" :candidates []}))))

(defn toggle-format-on-save [s]
  (let [on (not (:format-on-save s))]
    (-> s (assoc :format-on-save on)
          (msg (str "Format on save " (if on "on" "off"))))))

(defn- cli-format [s]
  (let [b (cur s)
        out (files/cli-formatted-text b)]
    (cond
      (nil? (:language b)) (msg s "No formatter for this buffer")
      out (-> s (update-cur #(buf/replace-text % out)) (msg "Formatted"))
      :else (msg s "No formatter available or nothing changed"))))

(defn format-buffer
  "C-c f: format with the language server when it is installed, else with the
   language's CLI formatter."
  [s]
  (let [sess (language/lsp-session s)]
    (if (and sess (not (:large-file (cur s))) (lsp/on-path? (:argv sess)))
      (lsp-format s)
      (cli-format s))))

(defn load-theme
  "M-x load-theme: switch palette (:default or :light) and keep it in the
   session, so a restart shows the same colours."
  [s]
  (mini-start s "Theme: "
    (fn [s input]
      (let [t (keyword (str/trim input))]
        (if (view/use-theme! t)
          (assoc s :theme t :msg (str "Theme: " (clojure.core/name t)))
          (msg s (str "Unknown theme. Available: "
                      (str/join ", " (view/theme-names)))))))
    (fn [input _]
      (mini/complete-prefix input (map clojure.core/name (view/theme-names))))))

;; --- Config reload ---

(defn- nesmacs-path [name]
  (str (System/getProperty "user.home") "/.nesmacs/" name))

(defn reload-config
  "M-x reload-config: re-run ~/.nesmacs/init.clj. Like M-:, it evaluates against
   a private atom and hands the result back, so api/bind! can't re-enter the
   command loop's swap!."
  [s]
  (let [path (nesmacs-path "init.clj")]
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
    (if-let [[_ path] (re-find #"diff --git a/(.+) b/" line)]
      (if-let [dir (git/repo-root (git/current-dir b))]
        (-> s (find-file (str dir "/" path))
              (cmd/msg (str "Opened " path)))
        (cmd/msg s "Not a git repository"))
      s)))

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

(defn- builtin-command-specs []
  [["forward-char" forward-char "Move point forward one character." "C-f" "<right>"]
   ["backward-char" backward-char "Move point backward one character." "C-b" "<left>"]
   ["next-line" next-line "Move to the next line." "C-n" "<down>"]
   ["previous-line" previous-line "Move to the previous line." "C-p" "<up>"]
   ["beginning-of-line" beginning-of-line "Move to start of current line." "C-a" "<home>"]
   ["end-of-line" end-of-line "Move to end of current line." "C-e" "<end>"]
   ["forward-word" forward-word "Move forward one word." "M-f"]
   ["backward-word" backward-word "Move backward one word." "M-b"]
   ["beginning-of-buffer" beginning-of-buffer "Move to start of buffer." "M-<"]
   ["end-of-buffer" end-of-buffer "Move to end of buffer." "M->"]
   ["scroll-down" scroll-down "Scroll forward one page." "C-v" "<next>"]
   ["scroll-up" scroll-up "Scroll backward one page." "M-v" "<prior>"]
   ["insert-newline" insert-newline "Insert a newline at point." "RET"]
   ["upcase-region" upcase-region "Convert the region to uppercase." "C-x C-u"]
   ["downcase-region" downcase-region "Convert the region to lowercase." "C-x C-l"]
   ["capitalize-region" capitalize-region "Capitalize words in the region."]
   ["mark-whole-buffer" mark-whole-buffer "Select the whole buffer." "C-x h"]
   ["exchange-point-and-mark" exchange-point-and-mark "Exchange point and mark." "C-x C-x"]
   ["transpose-chars" transpose-chars "Transpose characters around point." "C-t"]
   ["transpose-lines" transpose-lines "Transpose this line and the previous line." "C-x C-t"]
   ["comment-region" comment-region "Comment each line in the region." "M-;"]
   ["delete-char" delete-char "Delete the character after point." "C-d"]
   ["delete-backward-char" delete-backward-char "Delete the character before point." "DEL"]
   ["kill-line" kill-line "Kill from point to end of line." "C-k"]
   ["kill-region" kill-region "Kill the region between mark and point." "C-w"]
   ["yank" yank "Reinsert the most recent kill." "C-y"]
   ["set-mark" set-mark "Set the mark at point." "C-SPC" "C-@"]
   ["undo" undo "Undo the last change." "C-/"]
   ["redo" redo "Redo the last undone change." "M-/"]
   ["keyboard-quit" keyboard-quit "Cancel the current input or minibuffer." "C-g"]
   ["save-buffer" save-buffer "Save the current buffer to its file." "C-x C-s"]
   ["split-window-below" split-window-below "Split the current window into top and bottom." "C-x 2"]
   ["split-window-right" split-window-right "Split the current window into left and right." "C-x 3"]
   ["other-window" other-window "Switch to the next window." "C-x o"]
   ["delete-window" delete-window "Delete the current window." "C-x 0"]
   ["delete-other-windows" delete-other-windows "Delete all windows except the current one." "C-x 1"]
   ["enlarge-window" enlarge-window "Make the current window one row taller." "C-x ^"]
   ["shrink-window" shrink-window "Make the current window one row shorter."]
   ["enlarge-window-horizontally" enlarge-window-horizontally "Make the current window one column wider." "C-x }"]
   ["shrink-window-horizontally" shrink-window-horizontally "Make the current window one column narrower." "C-x {"]
   ["bookmark-set" bookmark-set "Save point as a named bookmark." "C-x r m"]
   ["bookmark-jump" bookmark-jump "Jump to a named bookmark." "C-x r b"]
   ["recentf-open" recentf-open "Open a recently visited file."]
   ["name-last-kbd-macro" name-last-kbd-macro-cmd "Save the last recorded macro under a name."]
   ["execute-kbd-macro" execute-kbd-macro-cmd "Replay a named keyboard macro."]
   ["query-replace" query-replace-cmd "Interactively replace literal matches." "M-%"]
   ["query-replace-regexp" query-replace-regexp-cmd "Interactively replace regex matches (use \\1 etc. in replacement)."]
   ["project-find-file" project-find-file "Find a file in the current project by fuzzy name." "C-x p f"]
   ["project-search" project-search "Search the current project with ripgrep." "C-x p g"]
   ["occur" occur-cmd "List lines matching a regex in the current buffer." "M-s o"]
   ["imenu" imenu "List definitions in the current buffer." "M-i"]
   ["reload-config" reload-config "Re-run ~/.nesmacs/init.clj."]
   ["hub" packagehub/open "Explore and manage editor extensions."]
   ["package-catalog" packagehub/set-source "Set the extension catalog URL or file."]
   ["package-update-all" packagehub/update-all "Check for and stage all installed package updates."]
   ["package-upgrade-all" packagehub/update-all "Check for and stage all installed package updates."]
   ["terminal" terminal-open "Open a terminal buffer running $SHELL." "C-c t"]
   ["agent" agent-cli "Open a terminal running the AI CLI ($NESMACS_AGENT or claude)."]
   ["debug" debug-start "Start the language's debug adapter for this file." "C-c d d"]
   ["debug-breakpoint" debug-breakpoint "Toggle a breakpoint on this line." "C-c d b"]
   ["debug-continue" debug-continue "Resume the debuggee." "C-c d c"]
   ["debug-next" debug-next "Step over the next line." "C-c d n"]
   ["debug-step-in" debug-step-in "Step into the call on this line." "C-c d i"]
   ["debug-step-out" debug-step-out "Run until the current frame returns." "C-c d o"]
   ["debug-stack" debug-stack "Show the frames of the last stop." "C-c d s"]
   ["debug-variables" debug-variables "Show the locals of the stopped frame." "C-c d v"]
   ["debug-eval" debug-eval "Evaluate an expression in the stopped frame." "C-c d e"]
   ["debug-quit" debug-quit "Disconnect the debug session." "C-c d q"]
   ["load-theme" load-theme "Switch the colour palette (default, light)."]
   ["complete-symbol-at-point" complete-symbol-at-point "Complete the symbol at point." "C-c i"]
   ["git-status" git-status "Show git status for the current project." "C-x g"]
   ["git-commit" gitcommit/commit-at-point "Commit staged changes."]
   ["git-diff" diff-open-at-point "Show diff for the entry at point."]
   ["git-log" gitlog/show-commit "Show commit log."]
   ["git-blame" git-blame "Show blame for the current file." "C-x v b"]
   ["git-stage-hunk" git-stage-hunk "Stage a hunk of the current file." "C-x v s"]
   ["git-unstage-hunk" git-unstage-hunk "Unstage a hunk of the current file." "C-x v u"]
   ["run-tests" run-tests "Run the project's tests and parse TAP/JUnit output."]
   ["lsp-hover" lsp-hover "Show hover information at point." "C-c h"]
   ["lsp-definition" lsp-definition "Jump to the definition at point." "M-."]
   ["lsp-references" lsp-references "List references to the symbol at point." "C-c r"]
   ["lsp-rename" lsp-rename "Rename the symbol at point across the project." "C-c R"]
   ["lsp-code-action" lsp-code-action "Choose a code action at point." "C-c a"]
   ["lsp-signature-help" lsp-signature-help "Show the signature at point." "C-c g"]
   ["lsp-document-symbols" lsp-document-symbols "List symbols from the language server." "C-c m"]
   ["format-buffer" format-buffer "Format the buffer (LSP, else CLI)." "C-c f"]
   ["format-region" lsp-format-region "Format the region through the language server." "C-c F"]
   ["toggle-format-on-save" toggle-format-on-save "Toggle formatting with the CLI formatter before save."]
   ["toggle-fold" toggle-fold "Fold or unfold the form at point." "C-c TAB"]
   ["fold-all" fold-all "Fold every range in the buffer."]
   ["unfold-all" unfold-all "Unfold every range in the buffer."]])

(defn- builtin-commands []
  (into {} (map (fn [[name f doc]] [name {:fn f :doc doc}])) (builtin-command-specs)))

(defn- register-builtin-commands [s]
  (assoc s :commands (builtin-commands)))

;; --- M-x ---

(defn- command-names [s]
  (sort (keys (:commands s))))

(defn- command-completer
  "Tab-complete command names. Returns {:completed str :candidates [str]}."
  [input s]
  (mini/complete-prefix input (command-names s)))

(defn execute-extended-command
  "Look up and invoke a named command. M-x entry point."
  [s name]
  (if (str/blank? name)
    s
    (let [n (str/trim name)
          cmd (get-in s [:commands n])]
      (if cmd ((:fn cmd) s) (msg s (str "No command: " n))))))

;; Register commands remain callable from user configuration.
(def point-to-register register/point-to-register)
(def jump-to-register register/jump-to-register)
(def copy-to-register register/copy-to-register)
(def insert-register register/insert-register)
(def window-configuration-to-register register/window-configuration-to-register)

;; --- Bookmarks (named, persisted positions) ---

(defn- bookmark-completer [input s]
  (mini/complete-prefix input (sort (map str (keys (:bookmarks s))))))

(defn- bookmark-set-1 [s name]
  (let [n (str/trim name)]
    (if (str/blank? n)
      s
      (let [bookmarks (assoc (:bookmarks s) n
                             {:buf (:buf s) :pos (:point (cur s))})]
        (-> s (assoc :bookmarks bookmarks)
              (api/persist-setting :bookmarks)
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
  (mini/complete-prefix input (:recentf s)))

(defn recentf-open [s]
  (mini-start s "Recent file: " find-file recentf-completer))

;; --- C-x r prefix map (registers + rectangles + bookmarks) ---

(def ^:private cx-r-map
  {\space point-to-register
   \j     jump-to-register
   \s     copy-to-register
   \i     insert-register
   \w     window-configuration-to-register
   \k     rect/kill-rectangle
   \M     rect/copy-rectangle
   \y     rect/yank-rectangle
   \c     rect/clear-rectangle
   \t     (prompt "String rectangle: " rect/string-rectangle)})

(def bindings
  (reduce (fn [keymap [_ f _ & paths]]
            (reduce (fn [m path] (keymap/bind m [] (keymap/parse-key-string path) f))
                    keymap paths))
          {[:meta \g] (prompt "Goto line: " goto-line)
           [:meta \:] (prompt "Eval: " eval-expression)
           [:meta \x] (prompt "M-x " execute-extended-command command-completer)
           [:meta \!] (prompt "Shell command: " async-shell-command)
           [:meta \|] (prompt "Shell command on region: " shell-command-on-region)
           [:ctrl \s] #(isearch-start % :forward)
           [:ctrl \r] #(isearch-start % :backward)
           [:ctrl \u] universal-argument
           [:ctrl \l] recenter
           [:ctrl \z] suspend
           [:ctrl \h] {\k describe-key
                       \f (prompt "Describe function: " describe-function command-completer)
                       \v (prompt "Describe variable: " describe-variable)}
           [:ctrl \x] {[:ctrl \f] (prompt "Find file: " find-file mini/file-completer)
                       [:ctrl \b] buflist/open
                       [:ctrl \c] confirm-quit
                       \b (prompt "Buffer: " switch-buffer)
                       \d (prompt "Dired: " dired/open mini/file-completer)
                       \k (prompt "Kill buffer: " kill-buffer)
                       \r cx-r-map
                       \( start-kbd-macro
                       \) end-kbd-macro
                       \e call-last-kbd-macro}}
          (builtin-command-specs)))

(def ^:private terminal-prefix
  "C-c is the only key the editor keeps inside a terminal. C-c C-c interrupts
   the process; C-c C-x opens the normal C-x map so the editor stays reachable."
  {[:ctrl \c] #(terminal/input % [:ctrl \c])
   [:ctrl \x] (get bindings [:ctrl \x])
   [:meta \x] (prompt "M-x " execute-extended-command command-completer)
   [:ctrl \b] buflist/open
   [:ctrl \k] #(kill-buffer % nil)
   [:ctrl \q] confirm-quit
   [:ctrl \r] terminal/restart})

(defn- dispatch-binding [s key binding prefixed?]
  (cond
    (map? binding) (assoc s :pending binding)
    (fn? binding) (cond-> (binding s)
                    (not= binding universal-argument) (dissoc :prefix-arg))
    prefixed? (if (= key [:ctrl \g])
                (keyboard-quit (dissoc s :prefix-arg))
                (msg s (str "prefix " key (if binding " not a command" " undefined"))))
    :else (dissoc (cond
                    (char? key) (if (>= (int key) 32) (self-insert s key) s)
                    (string? key) (self-insert s key)
                    :else (msg s (str key " undefined")))
                  :prefix-arg)))

(defn- cancel-escape? [s key]
  (and (= key :escape)
       (not (:query-replace s))
       (if-let [pending (:pending s)]
         (nil? (:escape pending))
         (or (:capture-next s)
             (:isearch s)
             (and (or (:mini s) (not= :terminal-mode (:mode (cur s))))
                  (nil? (resolve-binding s key)))))))

(defn handle-key
  "Pure state transition. Handles prefix keys via :pending state.
   Works identically for terminal and web — one key at a time."
  [s key]
  (let [cancel-escape (cancel-escape? s key)
        s (if cancel-escape s (-> s (dissoc :escape-count) (assoc :msg nil)))
        s (cond-> s (:macro-recording s) (update :macro-recording conj key))
        s' (cond
             cancel-escape
             (if (= 2 (:escape-count s))
               (keyboard-quit s)
               (update s :escape-count (fnil inc 0)))

             (and (map? key) (= :mouse (:type key)))
             (if (= :scroll (:action key)) (mouse-scroll s key) (mouse-click s key))

             ;; Bracketed paste is one insert, hence one undo entry. In a
             ;; terminal it belongs to the process, not to the read-only buffer.
             (and (vector? key) (= :paste (first key)))
             (cond
               (:isearch s) (search/isearch-append s (buf/normalize-eol (second key)))
               (= :terminal-mode (:mode (cur s))) (terminal/input s key)
               :else (cmd/insert-at-point s (buf/normalize-eol (second key)) true))

             ;; capture-next: consume the key as data, don't dispatch
             (= :describe-key (:capture-next s))
             (capture-describe-key (dissoc s :capture-next) key)

             (register/capturing? s) (register/capture s key)

             ;; pending prefix — resolve second key
             (:pending s)
             (dispatch-binding (dissoc s :pending) key (get (:pending s) key) true)

             (:query-replace s)
             (search/handle-replace s key)

             (:isearch s) (search/handle-isearch s key handle-key)

             ;; minibuffer-scoped keys
             (and (:mini s) (mini/keymap key)) ((mini/keymap key) s)

             ;; Terminal char mode: every key goes to the process except C-c,
             ;; the editor prefix. C-c C-x opens the normal C-x map so quitting,
             ;; visiting files and the buffer list stay reachable.
             (and (= :terminal-mode (:mode (cur s))) (not (:mini s)))
             (if (= [:ctrl \c] key)
               (assoc s :pending terminal-prefix)
               (terminal/input s key))

             :else
             (let [pa (:prefix-arg s)
                   digit? (and pa (char? key) (<= (int \0) (int key) (int \9)))]
               (cond
                 digit?
                 (assoc s :prefix-arg
                        {:num (+ (* 10 (or (:num pa) 0))
                                 (- (int key) (int \0)))})
                 :else
                 (dispatch-binding s key (resolve-binding s key) false))))]
    ;; Refresh live-filter candidates while a minibuffer is active.
    (cond-> s'
      (:mini s') mini/refresh-candidates)))

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

(defn- render-terminal! []
  ;; CAS the scroll writeback against the rendered snapshot so a concurrent
  ;; mutation (nREPL/web) doesn't clobber the other thread's update.
  (let [snapshot @editor
        {:keys [sig cur-scroll cur-hscroll font-lock-updates viewports]}
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
    (compare-and-set! editor snapshot updated)
    viewports))

(defn- commit-key! [key size clipboard!]
  (let [[before after] (swap-vals! editor
                         (fn [s]
                           (if size
                             (merge (run-key (merge s size) key) (select-keys s [:rows :cols]))
                             (run-key s key))))]
    (when-let [clip (cmd/new-clipboard-text before after)]
      (clipboard! clip))
    (session/journal-tick! editor)))

(defn command-loop [{:keys [active? input! render!]}]
  (loop [last-viewports nil]
    (when-let [[rows cols] (t/apply-pending-resize!)]
      (swap! editor assoc :rows rows :cols cols))
    (let [viewports (or (render-terminal!) last-viewports)
          key (t/read-key-timeout (if (and active? (active?)) 16 100))]
      (if key
        (commit-key! (if (= :mouse (:type key)) (assoc key :viewports viewports) key)
                     nil t/set-clipboard!)
        (session/journal-tick! editor))
      (when input! (input!))
      (api/run-effects! editor)
      ;; Suspend runs here, outside swap!, so no state lock is held while
      ;; stopped. The flag is cleared first so resume doesn't stop again.
      (when (:suspend-requested @editor)
        (swap! editor dissoc :suspend-requested)
        (t/suspend!))
      ;; Pump on every tick, including idle, so jobs stream while nothing is
      ;; being typed — and only from this thread. Draining is not pure, so it
      ;; happens before the swap: a CAS retry must not lose a chunk.
      (let [drained (job/drain! (:bufs @editor))]
        (when (seq drained) (swap! editor job/apply-drain drained)))
      (terminal/tick! editor tmux/backend)
      (complete/tick! editor)
      (files/auto-save-tick! editor)
      (session/save-tick! editor)
      (lsp/tick! editor)
      (job/consume-results! editor)
      (dap/tick! editor)
      (packagehub/tick! editor)
      (view/use-theme! (:theme @editor))
      (when (and render! (not (:exit @editor))) (render!))
      (when (:exit @editor)
        ;; Clean exit: the final snapshot includes every edit, so the journal
        ;; is only deleted after that write succeeds.
        (when (session/save-session! @editor)
          (session/discard-journal!)))
      (when-not (:exit @editor) (recur viewports)))))

(defn- run-all
  "Run each no-arg fn, logging and continuing on failure."
  [fns]
  (doseq [f fns]
    (try (f) (catch Exception e (log/log "shutdown:" (.getMessage e))))))

(def ^:private scratch-text
  (str ";; nesmacs\n"
       ";; C-x C-f open  |  C-x C-s save  |  C-x C-c quit\n"
       ";; M-x commands  |  M-: evaluate Clojure  |  C-g cancel\n\n"))

(defn- register-builtin-modes [s]
  (-> s
      (mode/register :fundamental :major)
      (mode/register :clojure-mode :major)
      (mode/register :emacs-lisp-mode :major)
      (mode/register :python-mode :major)
      (mode/register :javascript-mode :major)
      (mode/register :typescript-mode :major)
      (mode/register :tsx-mode :major)
      (mode/register :dired-mode :major :keymap dired-keymap)
      (mode/register :buflist-mode :major :keymap buflist-keymap)
      (mode/register :job-mode :major :keymap job-keymap)
      (mode/register :terminal-mode :major)
      (mode/register :nav-mode :major :keymap nav-keys)
      (mode/register :rg-mode :major :keymap rg-keys
                     :hint "RET jump  |  g rerun  |  C-c C-k stop")
      (mode/register :gitstatus-mode :major :keymap gitstatus-keymap)
      (mode/register :gitdiff-mode :major :keymap gitdiff-keymap)
      (mode/register :gitlog-mode :major :keymap gitlog-keymap)
      (mode/register :hunks-mode :major :keymap githunk/hunks-keymap)
      (mode/register :gitcommit-mode :major :keymap gitcommit/gitcommit-keymap
                     :hint "C-c C-c commit  |  C-c C-k abort")
      (mode/register :packagehub-mode :major
                     :keymap {:return packagehub/select-entry
                              :tab packagehub/switch-view
                              \/ packagehub/search
                              \g packagehub/refresh-catalog
                              \U packagehub/update-all
                              \s packagehub/set-source
                              \q packagehub/back}
                     :hint "RET select  U update all  TAB views  / search  g refresh  s catalog  q back")))

(defn- initial-state [rows cols]
  (-> {:buf "*scratch*"
       :bufs {"*scratch*" (buf/make "*scratch*" scratch-text nil)}
       :kill [] :msg nil :mini nil :mini-history []
       :windows (win/leaf "*scratch*") :cur-window []
       :rows rows :cols cols
       :theme :default
       :completion-sources
       (into {:default [complete/buffers complete/buffer-symbols]}
             (for [[_ l] lang/languages
                   :when (:lsp l)]
               [(:mode l) [complete/buffer-symbols complete/project-symbols
                           lsp-completion-source]]))}
      register-builtin-modes
      register-builtin-commands))

(defn- sanitize-persisted [bookmarks recentf history]
  {:bookmarks (session/sanitize-bookmarks bookmarks)
   :recentf (vec (take files/recentf-limit (session/sanitize-strings recentf)))
   :mini-history (session/sanitize-strings history)})

(defn- load-persisted-state!
  "Restore bookmarks, recentf, and mini-history from ~/.nesmacs/. Missing or
  corrupt files are logged and ignored."
  []
  (swap! editor merge
         (sanitize-persisted (persist/load! "bookmarks")
                             (persist/load! "recentf")
                             (persist/load! "mini-history"))))

(defn- load-init-files! []
  (let [load! (fn [name loader]
                (let [p (nesmacs-path name)]
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

(defn- web-start
  "Start the local web client. A taken port or load failure disables the client."
  []
  (try (let [port (Integer/parseInt (or (System/getenv "NESMACS_WEB_PORT") "1234"))]
         ((requiring-resolve 'nesmacs.web/start!) editor port commit-key!))
       (catch Exception e
         (log/log "web: disabled:" (.getMessage e))
         nil)))

(defn- web-stop [srv]
  (when srv ((requiring-resolve 'nesmacs.web/stop!) srv)))

(defn- start-services! []
  (let [preferred-port (try (Integer/parseInt (or (System/getenv "NESMACS_NREPL_PORT")
                                                  (System/getProperty "nesmacs.nrepl.port")
                                                  "7888"))
                          (catch Exception _ 7888))
        nrepl-port (try
                     (nrepl/start-server :port preferred-port)
                     (catch java.net.BindException _
                       (nrepl/start-server :port 0)))]
    (log/log "nrepl:" (:port nrepl-port) " dev:watching")
    (persist/save-quiet! "nrepl-port" (:port nrepl-port))
    {:nrepl nrepl-port
     :web   (web-start)
     :dev   (dev/start-watcher!)}))

(defn- stop-services! [{:keys [nrepl web dev]}]
  (run-all [#(terminal/shutdown-all! tmux/backend)
            dap/shutdown-all!
            #(dev/stop-watcher! dev)
            #(web-stop web)
            #(nrepl/stop-server nrepl)]))

(defn- open-launch-paths [s paths]
  ;; Keep the first argument visible while opening the rest as buffers.
  (reduce (fn [s path]
            (try (find-file s path)
                 (catch Exception e (msg-error s (str "Cannot open " path) e))))
          s
          (reverse paths)))

(defn- run-editor! [paths]
  (log/init!) (log/log "nesmacs starting")
  (t/install-shutdown-hook!)
  (proc/install-shutdown-hook!)
  (try
    (t/enter-raw-mode!) (t/enable-bracketed-paste!) (t/enable-mouse!) (t/cls)
    (t/on-resize!)
    (let [[rows cols] (t/terminal-size)]
      (reset! editor (initial-state rows cols))
      ;; nREPL and any other out-of-band caller can use nesmacs.api directly.
      (alter-var-root #'api/*state* (constantly editor))
      ;; Load session before init.clj so user config can override. A damaged
      ;; session or journal must never keep the editor from starting: fall back
      ;; to a fresh state and say so in the log.
      (try
        (when-let [sess (session/load-session!)]
          (let [[restored ok] (session/restore-session @editor sess)]
            (when ok
              (reset! editor (session/recover-journal! restored))
              (dap/restore-breakpoints! (:breakpoints sess)))))
        (catch Exception e
          (log/log "session restore failed:" (.getMessage e))
          (reset! editor (initial-state rows cols))))
      (reset! editor (-> @editor packages/initialize! packagehub/restore))
      (load-init-files!)
      (when (seq paths) (swap! editor open-launch-paths paths))
      (swap! editor files/recover-ask (files/pending-recoveries))
      ;; Baseline snapshot: the journal needs a version to replay from even if
      ;; the editor dies before the first periodic save.
      (session/save-session! @editor)
      (silence-stdio!)
      (let [services (start-services!)]
        (try (command-loop (:web services))
             (finally (stop-services! services)))))
    (finally
      (run-all [#(log/log "nesmacs exiting") lsp/shutdown-all! proc/kill-all!
                log/close! t/teardown! shutdown-agents]))))

(defn -main [& args]
  (if (and (= 1 (count args)) (#{"-h" "--help"} (first args)))
    (println "Usage: nesmacs [--] [FILE ...]")
    (run-editor! (if (= "--" (first args)) (rest args) args))))
