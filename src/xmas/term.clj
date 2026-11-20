(ns xmas.term
  (:require [clojure.string :as str]
            [xmas.log :as log])
  (:import [java.io InputStream]
           [sun.misc Signal SignalHandler]))

;; Raw terminal via stty + System.in/out. No JLine.

(defn- sh [^String cmd]
  (.exec (Runtime/getRuntime) (into-array String ["sh" "-c" cmd])))

(defn stty! [& args]
  (.waitFor (sh (str "stty " (str/join " " args) " </dev/tty"))))

(defn terminal-size []
  (let [out (slurp (.getInputStream (sh "stty size </dev/tty")))
        [r c] (str/split (str/trim out) #"\s+")]
    [(Integer/parseInt r) (Integer/parseInt c)]))

(defn enter-raw-mode! [] (stty! "raw" "-echo" "-icanon" "-isig"))
(defn exit-raw-mode! [] (stty! "cooked" "echo" "icanon" "isig"))

(defn on-resize!
  "Register a callback (fn [rows cols]) for terminal resize (SIGWINCH)."
  [callback]
  (let [handler (proxy [SignalHandler] []
                  (handle [_sig]
                    (try
                      (let [[rows cols] (terminal-size)]
                        (callback rows cols))
                      (catch Exception e
                        (log/log "resize error:" (.getMessage e))))))]
    (Signal/handle (Signal. "WINCH") handler)))

;; --- Input from System.in ---

(def ^InputStream stdin System/in)

(defn byte-available? [] (> (.available stdin) 0))

(defn read-byte-timeout ^long [^long ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (cond
        (byte-available?) (let [b (.read stdin)] (log/log "byte-t:" b) b)
        (>= (System/currentTimeMillis) deadline) -1
        :else (let [remaining (- deadline (System/currentTimeMillis))]
                (Thread/sleep (min 10 (max 1 remaining)))
                (recur))))))

(defn- read-utf8
  "Decode a multi-byte UTF-8 sequence given the leading byte."
  [^long b]
  (let [[n mask] (cond (< b 0xC0) [0 0]
                       (< b 0xE0) [1 0x1F]
                       (< b 0xF0) [2 0x0F]
                       :else      [3 0x07])]
    (when (pos? n)
      (let [cp (loop [i 0 cp (bit-and b mask)]
                 (if (>= i n) cp
                   (let [c (read-byte-timeout 50)]
                     (if (or (< c 0x80) (>= c 0xC0))
                       -1
                       (recur (inc i) (bit-or (bit-shift-left cp 6)
                                              (bit-and c 0x3F)))))))]
        (when (pos? cp)
          (if (<= cp 0xFFFF) (char cp)
            (String. (Character/toChars cp))))))))

;; --- Bracketed paste ---
;; Terminals wrap a paste in ESC[200~ ... ESC[201~ so the editor can treat it
;; as one insert instead of thousands of keystrokes (and one undo step).

(def ^:private paste-end (byte-array [27 91 50 48 49 126]))

(defn- read-paste-from
  "Read a bracketed-paste payload from `next-byte` (returns -1 at EOF).
   Returns the text without the markers; a truncated paste still returns what
   arrived, with any partial terminator kept as literal text."
  [next-byte]
  (let [out (java.io.ByteArrayOutputStream.)]
    (loop [matched []]
      (let [b (next-byte)]
        (cond
          (= -1 b)
          (do (doseq [m matched] (.write out (int m)))
              (String. (.toByteArray out) "UTF-8"))

          (= b (aget paste-end (count matched)))
          (if (= 5 (count matched))
            (String. (.toByteArray out) "UTF-8")
            (recur (conj matched b)))

          :else
          (do (doseq [m matched] (.write out (int m)))
              (if (= b (aget paste-end 0))
                (recur [b])
                (do (.write out b) (recur [])))))))))

(defn- read-paste [] (read-paste-from #(read-byte-timeout 200)))

(defn- parse-csi []
  (loop [params ""]
    (let [b (read-byte-timeout 50)]
      (cond
        (< b 0) nil
        (<= 64 b 126)
        (case (char b)
          \A :up, \B :down, \C :right, \D :left, \H :home, \F :end
          \~ (case params
               "2"   :insert
               "3"   :delete
               "5"   :page-up
               "6"   :page-down
               "200" [:paste (read-paste)]
               nil)
          nil)
        :else (recur (str params (char b)))))))

(defn- parse-escape []
  (let [b (read-byte-timeout 50)]
    (cond
      (< b 0)   :escape
      (= b 91)  (parse-csi)
      (= b 79)  (let [b2 (read-byte-timeout 50)]
                  (if (< b2 0) nil
                    (case b2 80 :f1 81 :f2 82 :f3 83 :f4 nil)))
      (>= b 32) [:meta (char b)]
      (< b 27)  [:meta [:ctrl (char (+ b 96))]]
      :else nil)))

(defn- byte->key
  "Map a leading byte to a key value."
  [^long b]
  (cond
    (< b 0)   nil
    (= b 27)  (parse-escape)
    (= b 9)   :tab
    (= b 13)  :return
    (= b 10)  :return
    (= b 127) :backspace
    ;; NUL is what a terminal sends for C-SPC / C-@ (byte 0), not "C-`".
    (= b 0)   [:ctrl \space]
    (< b 27)  [:ctrl (char (+ b 96))]
    (< b 32)  nil
    (< b 128) (char b)
    :else     (read-utf8 b)))

(defn read-key-timeout
  "Like read-key but returns nil after ms instead of blocking forever."
  [ms]
  (let [b (read-byte-timeout ms)]
    (when (>= b 0)
      (let [key (byte->key b)]
        (log/log "key-t:" (pr-str key))
        key))))

;; --- Output to System.out ---

(def ^:private E "\033[")
(def ^java.io.PrintStream real-out System/out)
(def ^:dynamic *out-fn* (fn [^String s] (.print real-out s)))

(defn tw [s] (*out-fn* (str s)))
(defn flush! [] (.flush real-out))

(defn enable-bracketed-paste! [] (tw "\033[?2004h"))
(defn- disable-bracketed-paste! [] (tw "\033[?2004l"))

;; --- Clipboard (OSC 52) ---

(defn clipboard-seq
  "OSC 52 clipboard copy. Terminals that support it (including over SSH) copy
   to the system clipboard; others ignore it. Terminals do not allow read-back."
  [^String s]
  (str "\033]52;c;"
       (.encodeToString (java.util.Base64/getEncoder) (.getBytes s "UTF-8"))
       "\u0007"))

(def ^:private clipboard-limit 100000)

(defn set-clipboard!
  "Copy `s` to the system clipboard. shortcut: capped at 100k chars, larger
   kills stay in the kill ring only; chunk if a real need appears."
  [^String s]
  (when (and (string? s) (<= (count s) clipboard-limit))
    (tw (clipboard-seq s))))

(defmacro with-frame
  "Batch all tw output into a single write+flush."
  [& body]
  `(let [sb# (StringBuilder. 4096)
         outer# *out-fn*
         result# (binding [*out-fn* (fn [s#] (.append sb# s#))]
                   ~@body)]
     (outer# (.toString sb#))
     (.flush real-out)
     result#))

(defn- esc [& xs] (tw (apply str E xs)))

(defn move [r c] (esc (inc r) ";" (inc c) "H"))
(defn cls      [] (esc "2J" E "H"))
(defn show-cur [] (esc "?25h"))
(defn hide-cur [] (esc "?25l"))
(defn reset-sg [] (esc "0m"))

(defn sg [{:keys [fg bg bold]}]
  (esc (str/join ";" (cond-> [0] bold (conj 1) fg (conj 38 5 fg) bg (conj 48 5 bg))) "m"))

(defn teardown!
  "Restore the terminal to a sane state. Each step is isolated so a failure
   in one doesn't abort the rest."
  []
  (doseq [f [reset-sg cls #(move 0 0) show-cur flush!
             disable-bracketed-paste! exit-raw-mode!]]
    (try (f) (catch Exception _))))

(defn install-shutdown-hook!
  "Safety net: restore terminal even on unexpected JVM exit."
  []
  (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable teardown!)))
