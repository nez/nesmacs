(ns nesmacs.testutil
  "Shared filesystem fixtures and polling for integration tests."
  (:import [java.io File]
           [java.nio.file Files]))

(defn temp-dir
  ([] (temp-dir "nesmacs-test"))
  ([prefix]
   (doto (File/createTempFile prefix "")
     (.delete)
     (.mkdirs))))

(defn delete-tree
  "Delete `f` recursively without following symbolic links. A fixture that
   links to a shared install must not wipe that install on cleanup."
  [^File f]
  (when (and (.isDirectory f)
             (not (Files/isSymbolicLink (.toPath f))))
    (doseq [^File c (.listFiles f)] (delete-tree c)))
  (.delete f))

(defn pump-until!
  "Tick once before each check and measure the timeout with a monotonic clock."
  [tick! interval-ms editor pred timeout-ms]
  (let [start (System/nanoTime)
        timeout-ns (* timeout-ms 1000000)]
    (loop []
      (tick! editor)
      (cond
        (pred) true
        (>= (- (System/nanoTime) start) timeout-ns) false
        :else (do (Thread/sleep interval-ms) (recur))))))
