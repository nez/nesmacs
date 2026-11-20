(ns xmas.proc
  "Child processes launched from an argv vector.

   Output is buffered in a bounded queue and pulled by `drain!` from the
   editor loop, so reader threads never mutate editor state. Bounded queue
   means a fast producer blocks instead of eating memory.

   Never call this with a shell string: pass argv and let the caller decide
   whether that means [\"sh\" \"-c\" ...]."
  (:require [xmas.log :as log])
  (:import [java.io InputStreamReader]
           [java.util.concurrent LinkedBlockingQueue]))

(defonce ^:private procs (atom {}))
(defonce ^:private next-id (atom 0))
(def ^:private queue-depth 256)

(defn- reader-loop
  [^InputStreamReader r ^LinkedBlockingQueue q ^Process p on-exit]
  (let [buf (char-array 8192)]
    (try
      (loop []
        (let [n (.read r buf)]
          (if (neg? n)
            (on-exit (.waitFor p))
            (do (.put q (String. buf 0 n))
                (recur)))))
      (catch InterruptedException _ (on-exit -1))
      (catch Exception e
        (log/log "proc reader:" (.getMessage e))
        (on-exit -1)))))

(defn start
  "Start `argv` in `dir` (nil = JVM cwd), with optional `env` overrides.
   Returns the handle; output is read with `drain!`.

   By default stdin is closed: tools like rg/grep search stdin forever when
   they see a pipe that never reaches EOF. Pass :keep-stdin? true for
   long-lived servers (LSP) that read requests from stdin."
  [{:keys [argv ^String dir env keep-stdin?]}]
  (let [pb (ProcessBuilder. (into-array String argv))
        _  (when dir (.directory pb (java.io.File. dir)))
        _  (when env (.putAll (.environment pb)
                              (into {} (map (fn [[k v]] [(name k) (str v)])) env)))
        p  (.start pb)
        _  (when-not keep-stdin? (.close (.getOutputStream p)))
        id (swap! next-id inc)
        q  (LinkedBlockingQueue. queue-depth)
        exit (atom nil)
        on-exit (fn [code] (reset! exit code))
        t (doto (Thread. #(reader-loop (InputStreamReader. (.getInputStream p) "UTF-8")
                                       q p on-exit))
            (.setDaemon true)
            (.setName (str "xmas-proc-" id))
            (.start))]
    (swap! procs assoc id {:id id :process p :queue q :exit exit :thread t})
    {:id id :process p :queue q :exit exit :thread t}))

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
  "True once the process has exited and every buffered chunk was drained."
  [h]
  (and (some? @(:exit h)) (.isEmpty (:queue h))))

(defn kill!
  "Destroy `h` and unblock its reader. Never blocks the caller."
  [h]
  (when h
    (.clear (:queue h))
    (.interrupt ^Thread (:thread h))
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