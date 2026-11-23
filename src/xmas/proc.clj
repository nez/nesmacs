(ns xmas.proc
  "Child processes launched from an argv vector.

   Output is buffered in a bounded queue and pulled by `drain!` from the
   editor loop, so reader threads never mutate editor state. Bounded queue
   means a fast producer blocks instead of eating memory.

   Never call this with a shell string: pass argv and let the caller decide
   whether that means [\"sh\" \"-c\" ...]."
  (:require [clojure.string :as str]
            [xmas.log :as log])
  (:import [java.io InputStreamReader]
           [java.util.concurrent CountDownLatch LinkedBlockingQueue]))

(defonce ^:private procs (atom {}))
(defonce ^:private next-id (atom 0))
(def ^:private queue-depth 256)

(defn- pump
  "Read chunks from `r` and hand each decoded string to `consume` until EOF."
  [^InputStreamReader r consume]
  (let [buf (char-array 8192)]
    (loop []
      (let [n (.read r buf)]
        (when-not (neg? n)
          (consume (String. buf 0 n))
          (recur))))))

(defn- reader-loop
  [^InputStreamReader r ^LinkedBlockingQueue q ^Process p on-exit ^CountDownLatch done]
  (try
    (pump r #(.put q %))
    (on-exit (.waitFor p))
    (catch InterruptedException _ (on-exit -1))
    (catch Exception e
      (log/log "proc reader:" (.getMessage e))
      (on-exit -1))
    (finally (.countDown done))))

(defn- stderr-loop
  "Drain stderr so a chatty process can't fill the pipe and deadlock.
   :merge appends it to the job queue; :log keeps protocol pipes clean (LSP)."
  [^InputStreamReader r ^LinkedBlockingQueue q mode ^CountDownLatch done]
  (try
    (pump r (fn [s]
              (if (= mode :merge)
                (.put q s)
                (log/log "proc stderr:" (str/trim-newline s)))))
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
   passes :log to keep the JSON-RPC stream clean."
  [{:keys [argv ^String dir env keep-stdin? stderr]
    :or {stderr :merge}}]
  (let [pb (ProcessBuilder. (into-array String argv))
        _  (when dir (.directory pb (java.io.File. dir)))
        _  (when env (.putAll (.environment pb)
                              (into {} (map (fn [[k v]] [(name k) (str v)])) env)))
        p  (.start pb)
        _  (when-not keep-stdin? (.close (.getOutputStream p)))
        id (swap! next-id inc)
        q  (LinkedBlockingQueue. queue-depth)
        exit (atom nil)
        done (CountDownLatch. 2)
        on-exit (fn [code] (reset! exit code))
        t (doto (Thread. #(reader-loop (InputStreamReader. (.getInputStream p) "UTF-8")
                                       q p on-exit done))
            (.setDaemon true)
            (.setName (str "xmas-proc-" id))
            (.start))
        et (doto (Thread. #(stderr-loop (InputStreamReader. (.getErrorStream p) "UTF-8")
                                        q stderr done))
             (.setDaemon true)
             (.setName (str "xmas-proc-" id "-err"))
             (.start))
        h {:id id :process p :queue q :exit exit :done done
           :thread t :err-thread et}]
    (swap! procs assoc id h)
    h))

(defn by-id [id] (get @procs id))

(defn drain!
  "Drain buffered output, stopping once `limit` characters are buffered. A
   reader chunk is never split, so the result can exceed `limit` by up to
   one 8 KiB chunk. Non-blocking."
  [h limit]
  (let [sb (StringBuilder.)]
    (loop []
      (when (and (< (.length sb) (long limit))
                 (when-let [s (.poll (:queue h))]
                   (.append sb ^String s)
                   true))
        (recur)))
    (.toString sb)))

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
  "Kill processes no buffer refers to any more (e.g. its buffer was killed
   through the buffer list instead of C-c C-k)."
  [live-ids]
  (doseq [[id h] @procs]
    (when-not (contains? live-ids id) (kill! h))))

(defn kill-all! []
  (doseq [[_ h] @procs] (kill! h))
  (reset! procs {}))

(defonce ^:private hook-installed? (atom false))

(defn install-shutdown-hook!
  "No zombies when the editor exits, including on SIGTERM."
  []
  (when (compare-and-set! hook-installed? false true)
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. ^Runnable #(try (kill-all!)
                                               (catch Exception _))))))