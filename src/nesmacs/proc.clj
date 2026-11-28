(ns nesmacs.proc
  "Child processes launched from an argv vector.

   Output is buffered in a bounded queue and pulled by `drain!` from the
   editor loop, so reader threads never mutate editor state. Bounded queue
   means a fast producer blocks instead of eating memory.

   Never call this with a shell string: pass argv and let the caller decide
   whether that means [\"sh\" \"-c\" ...]."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [nesmacs.log :as log])
  (:import [java.io ByteArrayOutputStream InputStream InputStreamReader]
           [java.lang ProcessBuilder$Redirect]
           [java.util Arrays]
           [java.util.concurrent CountDownLatch LinkedBlockingQueue]))

(defonce ^:private procs (atom {}))
(defonce ^:private next-id (atom 0))
(def ^:private queue-depth 256)

(defn- process-builder [argv dir]
  (cond-> (ProcessBuilder. (into-array String argv))
    dir (.directory (java.io.File. (str dir)))))

(defn run
  "Run `argv` synchronously and return its output. `opts` may set `:stdin`
   (text written to the child) and `:merge-stderr?`. With `:check-exit?`,
   a nonzero exit is reported as \"Error: ...\" followed by the command output."
  [argv dir {:keys [stdin merge-stderr? check-exit?]}]
  (try
    (let [p (.start (cond-> (process-builder argv dir)
                     merge-stderr? (.redirectErrorStream true)
                     (not merge-stderr?) (.redirectError ProcessBuilder$Redirect/DISCARD)))]
      (try
        (let [writer (when stdin
                       (future (with-open [w (io/writer (.getOutputStream p) :encoding "UTF-8")]
                                 (.write w ^String stdin))))]
          (when-not writer (.close (.getOutputStream p)))
          (let [out (slurp (.getInputStream p) :encoding "UTF-8")
                exit (.waitFor p)]
            (when writer @writer)
            (if (and check-exit? (not (zero? exit)))
              (str "Error: " (if (str/blank? out) (str "exit " exit) (str/trim out)))
              out)))
        (finally (when (.isAlive p) (.destroyForcibly p)))))
    (catch Exception e (str "Error: " (.getMessage e)))))

(defn- pump
  "Read chunks from `r` and hand each decoded string to `consume` until EOF."
  [^InputStreamReader r consume]
  (let [buf (char-array 8192)]
    (loop []
      (let [n (.read r buf)]
        (when-not (neg? n)
          (consume (String. buf 0 n))
          (recur))))))

(defn- pump-bytes [^InputStream input consume]
  (let [buffer (byte-array 8192)]
    (loop []
      (let [n (.read input buffer)]
        (when-not (neg? n)
          (consume (Arrays/copyOf buffer n))
          (recur))))))

(defn- reader-loop
  [^LinkedBlockingQueue q ^Process p on-exit ^CountDownLatch done binary?]
  (try
    (let [input (.getInputStream p)
          consume #(.put q %)]
      (if binary?
        (pump-bytes input consume)
        (pump (InputStreamReader. input "UTF-8") consume)))
    (on-exit (.waitFor p))
    (catch InterruptedException _ (on-exit -1))
    (catch Exception e
      (log/log "proc reader:" (.getMessage e))
      (on-exit -1))
    (finally (.countDown done))))

(defn- stderr-loop
  "Drain diagnostics separately from a protocol stream."
  [^InputStreamReader r ^CountDownLatch done]
  (try
    (pump r #(log/log "proc stderr:" (str/trim-newline %)))
    (catch InterruptedException _)
    (catch Exception e
      (log/log "proc stderr reader:" (.getMessage e)))
    (finally (.countDown done))))

(defn start
  "Start `argv` in `dir` (nil = JVM cwd), with optional `env` overrides.
   Returns the handle; output is read with `drain!`.

   By default stdin is closed: tools like rg/grep search stdin forever when
   they see a pipe that never reaches EOF. Pass :keep-stdin? true for
   long-lived servers (LSP) that read requests from stdin.

   stderr defaults to :merge so job buffers show compiler/test errors; LSP
   passes :log to keep the JSON-RPC stream clean. :binary? keeps stdout
   as bytes and requires :stderr :log.

   `kind` scopes orphan reaping: only a caller that owns a kind reaps it, so
   job cleanup cannot kill LSP servers or terminal clients."
  [{:keys [argv ^String dir env keep-stdin? stderr kind binary?]
    :or {stderr :merge}}]
  (when (and binary? (not= stderr :log))
    (throw (IllegalArgumentException. "Binary stdout requires :stderr :log")))
  (let [merged? (= stderr :merge)
        pb (doto (process-builder argv dir) (.redirectErrorStream merged?))
        _  (when env (.putAll (.environment pb)
                              (into {} (map (fn [[k v]] [(name k) (str v)])) env)))
        p  (.start pb)
        _  (when-not keep-stdin? (.close (.getOutputStream p)))
        id (swap! next-id inc)
        q  (LinkedBlockingQueue. queue-depth)
        exit (atom nil)
        done (CountDownLatch. (if merged? 1 2))
        on-exit (fn [code] (reset! exit code))
        t (doto (Thread. #(reader-loop q p on-exit done binary?))
            (.setDaemon true)
            (.setName (str "nesmacs-proc-" id))
            (.start))
        et (when-not merged?
             (doto (Thread. #(stderr-loop (InputStreamReader. (.getErrorStream p) "UTF-8") done))
               (.setDaemon true)
               (.setName (str "nesmacs-proc-" id "-err"))
               (.start)))
        h {:id id :process p :queue q :exit exit :done done :kind kind
           :thread t :err-thread et}]
    (swap! procs assoc id h)
    h))

(defn by-id [id] (get @procs id))

(defn- drain-chunks! [h limit size add!]
  (loop []
    (when (< (long (size)) (long limit))
      (when-let [chunk (.poll ^LinkedBlockingQueue (:queue h))]
        (add! chunk)
        (recur)))))

(defn drain!
  "Drain buffered text without blocking. The soft limit never splits a chunk."
  [h limit]
  (let [out (StringBuilder.)]
    (drain-chunks! h limit #(.length out) #(.append out ^String %))
    (.toString out)))

(defn drain-bytes! [h limit]
  (let [out (ByteArrayOutputStream.)]
    (drain-chunks! h limit #(.size out) #(.writeBytes out ^bytes %))
    (.toByteArray out)))

(defn exit-code [h] @(:exit h))

(defn finished?
  "True once the process has exited, both pipes are drained, and every
   buffered chunk was drained."
  [h]
  (and (some? @(:exit h))
       (zero? (.getCount ^CountDownLatch (:done h)))
       (.isEmpty (:queue h))))

(defn kill!
  "Destroy `h` and unblock its reader. Never blocks the caller."
  [h]
  (when h
    (.clear (:queue h))
    (.interrupt ^Thread (:thread h))
    (when-let [et (:err-thread h)] (.interrupt ^Thread et))
    (.destroyForcibly ^Process (:process h))
    (swap! procs dissoc (:id h))))

(defn forget!
  "Drop a finished handle once its output has been drained."
  [h]
  (when h (swap! procs dissoc (:id h))))

(defn kill-orphans!
  "Kill `kind` processes whose ids are not in `live-ids` (e.g. a job buffer
   killed through the buffer list instead of C-c C-k)."
  [kind live-ids]
  (doseq [[id h] @procs]
    (when (and (= kind (:kind h)) (not (contains? live-ids id)))
      (kill! h))))

(defn kill-all! []
  (doseq [[_ h] @procs] (kill! h))
  (reset! procs {}))

(defn executable?
  "True when the first element of `argv` resolves to an executable file: an
   absolute/relative path that runs, or a bare name found on PATH."
  [argv]
  (when-let [^String bin (first argv)]
    (let [f (java.io.File. bin)]
      (boolean
       (if (or (.isAbsolute f) (str/includes? bin java.io.File/separator))
         (.canExecute f)
         (some (fn [dir]
                 (let [c (java.io.File. ^String (str dir) bin)]
                   (and (.isFile c) (.canExecute c))))
               (str/split (or (System/getenv "PATH") "")
                          (re-pattern java.io.File/pathSeparator))))))))

(defn free-port
  "An unused local TCP port, for adapters that listen instead of using stdio.
   Racy by nature: the caller binds it right after."
  []
  (with-open [s (java.net.ServerSocket. 0)]
    (.getLocalPort s)))

(defonce ^:private hook-installed? (atom false))

(defn install-shutdown-hook!
  "No zombies when the editor exits, including on SIGTERM."
  []
  (when (compare-and-set! hook-installed? false true)
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. ^Runnable #(try (kill-all!)
                                               (catch Exception _))))))
