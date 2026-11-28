(ns nesmacs.terminal
  "Terminal buffers. The editor owns text and props; a backend owns the PTY and
   the VT emulation. `nesmacs.terminal.tmux` is the first backend: it keeps one
   tmux control-mode client per terminal, so the shell survives an editor crash
   and can be reattached.

   Pure here: key queues, SGR parsing, snapshot -> buffer, capture scheduling.
   The backend does all I/O and is polled from `tick!` on the editor loop.
   Nothing in `handle-key` writes to a process."
  (:require [clojure.string :as str]
            [nesmacs.buf :as buf]
            [nesmacs.cmd :as cmd]
            [nesmacs.gap :as gap]
            [nesmacs.log :as log]
            [nesmacs.props :as props]
            [nesmacs.text :as text]
            [nesmacs.window :as win])
  (:import [java.util UUID]))

(defprotocol Backend
  (attach! [backend spec]
    "Start/attach a session. spec has :id :argv :dir :cols :rows :create?.
     Returns a handle or nil when the backend is unavailable.")
  (handles [backend]
    "Map of session id -> handle for every live client.")
  (drain! [backend handle]
    "Read pending output. Returns {:events [...] :dirty? bool :exited? bool}.
     A :snapshot event is {:type :snapshot :lines [..] :x n :y n :title s}.")
  (send! [backend handle keys]
    "Encode and queue editor keys (chars, strings, [:ctrl c], :return, ...).")
  (resize! [backend handle cols rows])
  (capture! [backend handle]
    "Queue a screen + cursor capture; it arrives as a :snapshot event.")
  (close! [backend handle]
    "Close the client but leave the session running (editor exit).")
  (kill! [backend handle]
    "Close the client and destroy the session (buffer killed)."))

;; --- Faces and SGR ---

(def ^:private default-face {:fg nil :bg nil :bold false})

(defn- parse-params [^String s]
  ;; parse-long returns nil on junk; -1 matches no branch, so an unknown
  ;; parameter is ignored instead of throwing inside a capture.
  (mapv (fn [p] (if (str/blank? p) 0 (or (parse-long (str/trim p)) -1)))
        (str/split s #";" -1)))

(defn- sgr-apply
  "Apply one SGR parameter string to `face`. Unsupported codes are ignored so a
   new attribute in a TUI cannot break the parser."
  [face params]
  (loop [f face i 0]
    (let [p (nth params i nil)]
      (cond
        (nil? p) f
        (= p 0)  (recur default-face (inc i))
        (= p 1)  (recur (assoc f :bold true) (inc i))
        (= p 22) (recur (assoc f :bold false) (inc i))
        (<= 30 p 37) (recur (assoc f :fg (- p 30)) (inc i))
        (= p 39) (recur (assoc f :fg nil) (inc i))
        (<= 40 p 47) (recur (assoc f :bg (- p 40)) (inc i))
        (= p 49) (recur (assoc f :bg nil) (inc i))
        (<= 90 p 97) (recur (assoc f :fg (+ 8 (- p 90))) (inc i))
        (<= 100 p 107) (recur (assoc f :bg (+ 8 (- p 100))) (inc i))
        (or (= p 38) (= p 48))
        (let [k (if (= p 38) :fg :bg)
              mode (nth params (inc i) nil)]
          (cond
            (= mode 5) (recur (assoc f k (nth params (+ i 2) nil)) (+ i 3))
            (= mode 2) (recur (assoc f k [(nth params (+ i 2) 0)
                                          (nth params (+ i 3) 0)
                                          (nth params (+ i 4) 0)])
                              (+ i 5))
            :else (recur f (inc i))))
        :else (recur f (inc i))))))

(defn- csi-end
  "Index of the final byte of the CSI sequence starting at `i`, or `n` when the
   line ends first."
  ^long [^String line ^long i]
  (let [n (.length line)]
    (loop [j (+ i 2)]
      (cond (>= j n) n
            (<= 64 (int (.charAt line j)) 126) j
            :else (recur (inc j))))))

(defn- append-span [spans start end face]
  (cond-> spans
    (and (< start end) (not= face default-face)) (conj [start end face])))

(defn parse-sgr-line
  "Strip SGR sequences from `line`. Returns [text spans], spans being
   [from to face] over `text` and default-face runs omitted."
  [^String line]
  (let [n (.length line)
        sb (StringBuilder.)]
    (loop [i 0 pos 0 face default-face segs [] seg-start 0]
      (if (>= i n)
        [(str sb) (append-span segs seg-start pos face)]
        (let [c (.charAt line i)]
          (if (and (= c \u001b) (< (inc i) n) (= \[ (.charAt line (inc i))))
            (let [end (csi-end line i)]
              (if (and (< end n) (= \m (.charAt line end)))
                (let [nf (sgr-apply face (parse-params (subs line (+ i 2) end)))]
                  (if (= nf face)
                    (recur (inc end) pos face segs seg-start)
                    (recur (inc end) pos nf
                           (append-span segs seg-start pos face)
                           pos)))
                ;; Non-SGR control sequence: drop it rather than sending an ESC
                ;; to the renderer, where it would only show as a replacement char.
                (recur (inc end) pos face segs seg-start)))
            (do (.append sb c)
                (recur (inc i) (inc pos) face segs seg-start))))))))

(defn- cursor-offset
  "Offset of tmux cursor (x = display column, y = row) in the snapshot text."
  ^long [texts offsets x y]
  (if (or (neg? (long y)) (>= (long y) (count texts)))
    (long (reduce + (map #(+ 1 (count %)) texts)))
    (let [line (nth texts y)]
      (+ (long (nth offsets y)) (text/pos-at-col line 0 (count line) (max 0 (long x)))))))

(defn apply-snapshot
  "Replace terminal buffer `b` with a captured screen. Pure. Returns `b`
   unchanged when the screen is identical, so an idle capture does not bump the
   version and force a repaint."
  [b lines x y title]
  (let [parsed  (mapv parse-sgr-line lines)
        texts   (mapv first parsed)
        offsets (loop [ts texts off 0 acc []]
                  (if-let [t (first ts)]
                    (recur (rest ts) (+ off (count t) 1) (conj acc off))
                    acc))
        content (str/join "\n" texts)
        point   (cursor-offset texts offsets x y)
        title   (when-not (str/blank? (str title)) (str title))
        spans   (into []
                  (mapcat (fn [[i [_ line-spans]]]
                            (let [base (long (nth offsets i))]
                              (map (fn [[from to face]]
                                     (props/make (+ base from) (+ base to) face))
                                   line-spans))))
                  (map-indexed vector parsed))]
    (if (and (= content (str (:text b)))
             (= point (long (:point b)))
             (= spans (:props b))
             (= title (:title b)))
      b
      (-> b
          (assoc :text (gap/of content)
                 :point point
                 :mark nil
                 :props spans
                 :modified false
                 :font-lock []
                 :font-lock-window nil
                 :large-file false
                 :version (inc (or (:version b) 0))
                 :title title)))))

;; --- State helpers ---

(defn terminal? [b] (boolean (:terminal b)))

(defn input
  "Queue one key for the terminal process. Pure; the loop sends it on tick."
  [s key]
  (if (terminal? (cmd/cur s))
    (update-in s [:bufs (:buf s) :terminal :in] (fnil conj []) key)
    s))

(defn shell-argv []
  (let [sh (System/getenv "SHELL")
        f  (when sh (java.io.File. sh))]
    (if (and f (.canExecute f)) [sh] ["sh"])))

(defn- unique-name [s]
  (if-not (contains? (:bufs s) "*terminal*")
    "*terminal*"
    (loop [n 2]
      (let [name (str "*terminal-" n "*")]
        (if (contains? (:bufs s) name) (recur (inc n)) name)))))

(defn- body-size
  "Columns and body rows within a window rectangle."
  [{:keys [cols rows]}]
  [(max 1 (long cols)) (max 1 (dec (long rows)))])

(defn open
  "Open a terminal buffer running `argv` (default: $SHELL) in the current
   directory. The client starts on the next `tick!`."
  ([s] (open s (shell-argv)))
  ([s argv]
   (let [layout (:layout (win/frame-geometry s))
         [cols rows] (body-size (get layout (:cur-window s) {:rows 24 :cols 80}))
         name (unique-name s)
         dir  (or (some-> (:file (cmd/cur s)) java.io.File. .getParent)
                  (System/getProperty "user.dir"))
         b    (-> (buf/make name "" nil)
                  (assoc :mode :terminal-mode :read-only true :journal? false
                         :props [] :font-lock [] :font-lock-window nil
                         :large-file false
                         :terminal {:id (str "nesmacs-" (subs (str (UUID/randomUUID)) 0 8))
                                    :argv (vec argv)
                                    :dir dir
                                    :status :starting
                                    :create? true
                                    :cols cols
                                    :rows rows
                                    :in []
                                    :dirty? true
                                    :last-capture 0}))]
     (-> s
         (assoc-in [:bufs name] b)
         (cmd/set-cur-buffer name)))))

(defn restart
  "Reattach the current terminal, creating its session again if it died."
  [s]
  (let [b (cmd/cur s)]
    (if-not (terminal? b)
      (cmd/msg s "Not a terminal buffer")
      (-> s
          (update-in [:bufs (:buf s) :terminal]
                     #(assoc % :status :starting :create? true
                               :dirty? true :in []))
          (assoc :msg (str "Restarting " (:name b)))))))

;; --- Editor-loop driver ---

(def ^:private capture-idle-ms 500)
(def ^:private max-keys-per-tick 128)

(defn- rect-for [s layout name]
  (if (and (= name (:buf s)) (contains? layout (:cur-window s)))
    (get layout (:cur-window s))
    ;; `win/leaves` yields paths inside the window tree, not the editor state.
    (some (fn [path]
            (when (= name (get-in (:windows s) (conj path :buffer)))
              (get layout path)))
          (win/leaves (:windows s)))))

(defn- swap-buffer!
  "Apply `f` to buffer `name` only if it is still the same terminal session."
  [editor name id f]
  (swap! editor
         (fn [s]
           (if (= id (get-in s [:bufs name :terminal :id]))
             (update-in s [:bufs name] f)
             s))))

(defn- set-terminal! [editor name id k v]
  (swap-buffer! editor name id #(assoc-in % [:terminal k] v)))

(defn- apply-events! [editor name id events]
  (doseq [ev events]
    (case (:type ev)
      :snapshot
      (swap-buffer! editor name id
                    (fn [b] (-> (apply-snapshot b (:lines ev) (:x ev) (:y ev) (:title ev))
                                (assoc-in [:terminal :capture-pending?] false))))
      :error
      (set-terminal! editor name id :capture-pending? false)
      nil)))

(defn- attach-term!
  "Start the backend client for one terminal buffer. `spec` decides whether the
   backend may create the session (`:create?`) or must attach to an existing one.
   Returns true on success."
  [backend editor name id spec]
  (if-let [_ (attach! backend spec)]
    (do (set-terminal! editor name id :status :running)
        (set-terminal! editor name id :dirty? true)
        true)
    (do (set-terminal! editor name id :status :dead)
        (swap! editor assoc :msg "Terminal unavailable (C-c C-r retries)")
        false)))

(defn- reconcile-orphans!
  "Kill sessions whose buffer no longer exists. Closing a window keeps the
   buffer, so this is the only place a buffer kill reaches the backend."
  [backend terms]
  (let [live (set (map (fn [[_ b]] (get-in b [:terminal :id])) terms))]
    (doseq [[id h] (handles backend)]
      (when-not (contains? live id)
        (try (kill! backend h)
             (catch Exception e (log/log "terminal kill:" (.getMessage e))))))))

(defn tick!
  "Editor-loop step. Polls backends, applies snapshots, sends queued keys and
   schedules a capture for the focused/visible terminal only. Never blocks on a
   response and never writes editor state from a backend thread."
  [editor backend]
  (let [s @editor
        layout (:layout (win/frame-geometry s))
        terms (filterv (comp terminal? val) (:bufs s))]
    (reconcile-orphans! backend terms)
    (doseq [[name b] terms]
      (let [{:keys [id status]} (:terminal b)
            h (get (handles backend) id)
            rect (rect-for s layout name)
            [want-cols want-rows] (some-> rect body-size)]
        (try
          (cond
            (= status :dead) nil

            ;; C-c C-r on a live terminal: drop the client and attach again
            ;; (new-session -A reuses the session when it still exists).
            (and h (= status :starting))
            (do (close! backend h)
                (attach-term! backend editor name id (:terminal b)))

            (nil? h)
            (when (#{:starting :detached} status)
              (attach-term! backend editor name id (:terminal b)))

            :else
            (let [{:keys [events dirty? exited?]} (drain! backend h)]
              (apply-events! editor name id events)
              (if exited?
                (do (close! backend h)
                    (swap-buffer! editor name id #(assoc-in % [:terminal :status] :dead))
                    (when (= name (:buf @editor))
                      (swap! editor assoc :msg
                             "Terminal session ended (C-c C-r restarts)")))
                (do
                  (when dirty? (set-terminal! editor name id :dirty? true))
                  ;; Keys go first so their echo can land in this same capture.
                  (let [ks (vec (take max-keys-per-tick
                                      (:in (get-in @editor [:bufs name :terminal] []))))]
                    (when (seq ks)
                      (send! backend h ks)
                      (swap-buffer! editor name id
                                    #(update-in % [:terminal :in]
                                                (fn [q] (vec (drop (count ks) q)))))))
                  (let [spec (:terminal (get-in @editor [:bufs name]))]
                    (when (and want-cols
                               (or (not= want-cols (:cols spec))
                                   (not= want-rows (:rows spec))))
                      (resize! backend h want-cols want-rows)
                      (swap-buffer! editor name id
                                    #(-> (assoc-in % [:terminal :cols] want-cols)
                                         (assoc-in [:terminal :rows] want-rows)))))
                  (let [t        (:terminal (get-in @editor [:bufs name]))
                        now      (System/currentTimeMillis)
                        since    (long (or (:last-capture t) 0))
                        focused? (= name (:buf s))
                        visible? (some? rect)
                        due?     (and (not (:capture-pending? t))
                                      (or (and focused?
                                               (or dirty? (:dirty? t)
                                                   (> (- now since) capture-idle-ms)))
                                          (and visible? (not focused?)
                                               (or dirty? (:dirty? t)
                                                   (> (- now since)
                                                      (* 2 capture-idle-ms))))))]
                    (when (and due? (not= :dead (:status t)))
                      (try (capture! backend h)
                           (swap-buffer! editor name id
                                         #(-> (assoc-in % [:terminal :capture-pending?] true)
                                              (assoc-in [:terminal :dirty?] false)
                                              (assoc-in [:terminal :last-capture] now)))
                           (catch Exception e
                             (log/log "terminal capture:" (.getMessage e))))))))))
          (catch Exception e
            (log/log "terminal tick:" name (.getMessage e))))))
    nil))

;; --- Shutdown ---

(defn shutdown-all!
  "Detach every client, leaving tmux sessions alive for the next start."
  [backend]
  (doseq [[_ h] (handles backend)]
    (try (close! backend h) (catch Exception _))))

(defn kill-all-sessions!
  "Test helper: close clients and destroy their sessions."
  [backend]
  (doseq [[_ h] (handles backend)]
    (try (kill! backend h) (catch Exception _))))
