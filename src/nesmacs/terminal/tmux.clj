(ns nesmacs.terminal.tmux
  "tmux control-mode backend for `nesmacs.terminal`.

   One `tmux -C` client per terminal session, attached to a private server
   (`-L nesmacs`) with its own config. Commands and responses are line-oriented;
   `%output` only marks the pane dirty. The screen comes from
   `capture-pane -p -e`, so tmux keeps doing the VT emulation and this side
   never grows a terminal emulator.

   Sessions are created detached and survive the client and the editor. Closing
   a client on exit leaves the shell running; `kill!` is reserved for the buffer
   being killed."
  (:require [clojure.string :as str]
            [nesmacs.log :as log]
            [nesmacs.proc :as proc]
            [nesmacs.terminal :as term])
  (:import [java.io File]
           [java.util.concurrent TimeUnit]))

(def ^:dynamic *socket* "nesmacs")
(def ^:dynamic *config* nil)

(def ^:private config-text
  (str "set -g status off\n"
       "set -g window-size manual\n"
       "set -g default-terminal tmux-256color\n"
       "set -g escape-time 0\n"
       "set -g history-limit 5000\n"
       "set -g mouse off\n"))

(defn- config-file ^String []
  (or *config*
      (str (System/getProperty "user.home") "/.nesmacs/tmux.conf")))

(defn- ensure-config!
  "Rewrite the private config each attach so required settings (window-size
   manual, escape-time 0) cannot go stale."
  []
  (let [f (File. (config-file))]
    (try (when-let [p (.getParentFile f)] (.mkdirs p))
         (spit f config-text :encoding "UTF-8")
         (catch Exception e (log/log "tmux config:" (.getMessage e))))))

;; --- Control client ---

(defonce ^:private clients (atom {})) ; id -> {:h :inbox :out :pending :block ...}

(defn- write-line! [client ^String line]
  (let [out (.getOutputStream ^Process (:process (:h client)))]
    (.write out (.getBytes (str line "\n") "UTF-8"))
    (.flush out)))

(defn- command!
  "Queue one control command and its response token. Commands written before
   `%session-changed` wait, so a fast first keystroke is not lost."
  [client line token]
  (when client
    (if (:attached? client)
      (do (write-line! client line)
          (swap! clients update (:id client) update :pending conj token))
      (swap! clients update (:id client) update :out conj [line token]))))

;; --- Key encoding ---

(def ^:private special-keys
  {:return "Enter" :tab "Tab" :backspace "BSpace" :escape "Escape"
   :up "Up" :down "Down" :left "Left" :right "Right"
   :home "Home" :end "End" :insert "IC" :delete "DC"
   :page-up "PPage" :page-down "NPage"
   :f1 "F1" :f2 "F2" :f3 "F3" :f4 "F4"})

(defn- modifier-name [mod ch]
  (let [prefix (if (= mod :ctrl) "C-" "M-")
        c (if (= ch \space) "Space" (str (Character/toLowerCase ^char ch)))]
    (str prefix c)))

(defn key-name
  "tmux key name for a special/editor key, or nil when it is literal text."
  [k]
  (cond
    (contains? special-keys k) (special-keys k)
    (and (vector? k) (= 2 (count k)) (= :ctrl (first k))) (modifier-name :ctrl (second k))
    (and (vector? k) (= 2 (count k)) (= :meta (first k))) (modifier-name :meta (second k))
    :else nil))

(defn- hex-bytes [^String s]
  (mapv #(format "%02x" (bit-and 0xff (long %))) (.getBytes s "UTF-8")))

(defn- send-bytes! [client ^String s]
  ;; -H carries raw bytes, so spaces, quotes and newlines need no quoting and
  ;; cannot be mistaken for tmux command syntax.
  (doseq [chunk (partition-all 256 (hex-bytes s))]
    (command! client
              (str "send-keys -t " (:id client) " -H " (str/join " " chunk))
              :ignore)))

(defn- flush-text! [client ^StringBuilder sb]
  (when (pos? (.length sb))
    (let [s (.toString sb)]
      (.setLength sb 0)
      (send-bytes! client s))))

(defn send!
  [_ handle keys]
  (when-let [client (get @clients (:id handle))]
    (let [sb (StringBuilder.)]
      (doseq [k keys]
        (cond
          (and (vector? k) (= :paste (first k)))
          (do (flush-text! client sb)
              (send-bytes! client (str "\u001b[200~" (second k) "\u001b[201~")))

          (or (char? k) (string? k))
          (.append sb (str k))

          (key-name k)
          (do (flush-text! client sb)
              (command! client (str "send-keys -t " (:id client) " " (key-name k)) :ignore))

          :else (log/log "terminal: unencodable key" (pr-str k))))
      (flush-text! client sb))))

(defn resize!
  [_ handle cols rows]
  (when-let [c (get @clients (:id handle))]
    (command! c (str "resize-window -t " (:id c) " -x " (long cols) " -y " (long rows))
              :ignore)))

(defn capture!
  [_ handle]
  (when-let [c (get @clients (:id handle))]
    (command! c (str "capture-pane -p -e -t " (:id c)) :capture)
    (command! c (str "display-message -p -t " (:id c)
                     " \"#{cursor_x}\u001f#{cursor_y}\u001f#{pane_title}\"")
              :cursor)))

;; --- Line protocol ---

(defn- block-num [line]
  (let [parts (str/split line #"\s+")]
    (when (>= (count parts) 3) (parse-long (nth parts 2)))))

(defn- meta-of [lines]
  (let [line (first lines)
        [x y title] (str/split (or line "") #"\u001f" 3)]
    {:type :snapshot
     :x (or (parse-long (str/trim (or x ""))) 0)
     :y (or (parse-long (str/trim (or y ""))) 0)
     :title title}))

(defn- finish-block
  "Complete the current response. Returns [client events]. The first block after
   attach is the attach/new-session response, which carries no token; popping a
   queued token for it would misalign every later response."
  [client ok? events]
  (let [block (:block client)
        startup? (:startup block)
        token (when-not startup? (first (:pending client)))
        client (-> client
                   (assoc :block nil)
                   (update :pending #(if (and (seq %) (not startup?))
                                       (subvec % 1) %)))]
    (if (and token block)
      (if ok?
        (case token
          :capture [(assoc client :lines (:lines block)) events]
          :cursor (let [lines (:lines client)
                        meta (meta-of (:lines block))]
                    [(dissoc client :lines)
                     (cond-> events lines (conj (assoc meta :lines lines)))])
          [client events])
        ;; A failed capture must not leave the loop waiting on it forever.
        [(dissoc client :lines) (conj events {:type :error})])
      [client events])))

(defn- step-line
  "Fold one protocol line into the client. Returns [client events dirty? exited?]."
  [client line events dirty? exited?]
  (let [block (:block client)]
    (cond
      ;; Inside a response only the matching terminator ends it. Matching the
      ;; numeric id keeps a pane line that looks like \"%end ...\" from ending
      ;; the capture early, and tmux queues notifications until the block
      ;; closes, so every other line (even one starting with \"%output\" or
      ;; \"%exit\") is screen text.
      block
      (cond
        (and (or (str/starts-with? line "%end ") (str/starts-with? line "%error "))
             (= (block-num line) (:num block)))
        (let [[client events] (finish-block client (str/starts-with? line "%end ") events)]
          [client events dirty? exited?])

        :else
        [(assoc-in client [:block :lines] (conj (:lines block) line)) events dirty? exited?])

      (str/starts-with? line "%begin ")
      [(-> client
           (assoc :block {:num (block-num line) :lines []
                          :startup (not (:ready? client))})
           (assoc :ready? true))
       events dirty? exited?]

      (str/starts-with? line "%output ")
      [client events true exited?]

      (str/starts-with? line "%session-changed ")
      (let [client (assoc client :attached? true)
            client (reduce (fn [c [line token]]
                             (write-line! c line)
                             (update c :pending conj token))
                           client (:out client))]
        [(assoc client :out []) events dirty? exited?])

      (str/starts-with? line "%exit")
      [(assoc client :exited? true) events dirty? true]

      :else
      [client events dirty? exited?])))

(defn drain!
  [_ handle]
  (let [id (:id handle)
        c (get @clients id)]
    (if-not c
      {:events [] :dirty? false :exited? true}
      (let [chunk (proc/drain! (:h c) 1000000)
            inbox (str (:inbox c) chunk)
            lines (str/split inbox #"\n" -1)
            complete (butlast lines)
            rest* (last lines)
            [client events dirty? exited?]
            (reduce (fn [acc line]
                      (let [[client events dirty? exited?] acc]
                        (step-line client line events dirty? exited?)))
                    [(assoc c :inbox rest*) [] false false]
                    complete)
            exited? (or exited? (:exited? client) (proc/finished? (:h client)))]
        (swap! clients assoc id (assoc client :exited? exited?))
        {:events events :dirty? dirty? :exited? exited?}))))

;; --- Lifecycle ---

(defn- tmux-args [& args]
  (into ["tmux" "-L" *socket* "-f" (config-file)] args))

(defn- run-tmux!
  "One-shot tmux command with a short timeout; used for session cleanup."
  [& args]
  (try
    (let [p (.start (ProcessBuilder. (into-array String (apply tmux-args args))))]
      (.close (.getOutputStream p))
      (when-not (.waitFor p 1 TimeUnit/SECONDS) (.destroyForcibly p))
      true)
    (catch Exception e
      (log/log "tmux:" (.getMessage e))
      false)))

(defn attach!
  [_ {:keys [id argv dir env cols rows create?]}]
  (ensure-config!)
  (try
    (let [base (tmux-args "-C")
          args (if create?
                 (-> (into base ["new-session" "-A" "-s" id
                                 "-x" (str (long cols)) "-y" (str (long rows))])
                     (into (when dir ["-c" dir]))
                     (into (mapcat (fn [[key value]] ["-e" (str key "=" value)]) env))
                     (into (concat ["--"] argv)))
                 (into base ["attach" "-t" id]))
          h (proc/start {:argv (vec args) :dir dir :keep-stdin? true
                         :stderr :log :kind :terminal})
          client {:id id :h h :inbox "" :out [] :pending [] :block nil
                  :attached? false :dirty true :exited? false}]
      (swap! clients assoc id client)
      client)
    (catch Exception e
      (log/log "tmux attach:" (.getMessage e))
      nil)))

(defn handles [_] @clients)

(defn close!
  [_ handle]
  (when-let [c (get @clients (:id handle))]
    (proc/kill! (:h c))
    (swap! clients dissoc (:id handle))))

(defn kill!
  [_ handle]
  (let [id (:id handle)]
    (close! nil handle)
    (run-tmux! "kill-session" "-t" id)))

(defn shutdown-all! []
  (doseq [[_ c] @clients]
    (try (proc/kill! (:h c)) (catch Exception _)))
  (reset! clients {}))

(defn kill-all-sessions! []
  (doseq [[id _] @clients]
    (run-tmux! "kill-session" "-t" id))
  (shutdown-all!))

(def backend
  "Value passed to `nesmacs.terminal/tick!`."
  (reify term/Backend
    (attach! [b spec] (attach! b spec))
    (handles [b] (handles b))
    (drain! [b h] (drain! b h))
    (send! [b h ks] (send! b h ks))
    (resize! [b h c r] (resize! b h c r))
    (capture! [b h] (capture! b h))
    (close! [b h] (close! b h))
    (kill! [b h] (kill! b h))))
