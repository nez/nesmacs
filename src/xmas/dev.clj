(ns xmas.dev
  (:require [clojure.string :as str]
            [xmas.log :as log])
  (:import [java.nio.file FileSystems Path StandardWatchEventKinds]))

(defn ns-for-file
  "Convert src/xmas/foo.clj -> xmas.foo"
  [^String path]
  (when (.endsWith path ".clj")
    ;; Strip only the LEADING `src/` — `.replace` replaces every occurrence
    ;; which would chew up nested paths like `src/xmas/src/foo.clj`.
    (-> (if (.startsWith path "src/") (subs path 4) path)
        (.replace "/" ".")
        (.replace "_" "-")
        (.replace ".clj" "")
        symbol)))

(defn ns-for-change
  "Namespace for a changed file `full` inside the watched `src` root.
   Both paths must be absolute: a relative watchable used to yield
   `src/xmas/foo.clj`, i.e. the namespace `src.xmas.foo`, which is not on the
   classpath — every hot reload failed. nil for files outside the root."
  [^Path root ^Path full]
  (let [rel (try (.relativize root (.normalize full)) (catch Exception _ nil))]
    (when (and rel (not (str/starts-with? (str rel) "..")))
      (ns-for-file (str "src/" rel)))))

(defn reload! [ns-sym]
  (try
    (require ns-sym :reload)
    (log/log "dev: reloaded" ns-sym)
    (catch Exception e
      (log/log "dev: reload failed" ns-sym (.getMessage e)))))

(defn start-watcher!
  "Watch src/ for .clj changes, auto-reload namespaces. Returns {:future f :running a}."
  []
  (let [watcher (.newWatchService (FileSystems/getDefault))
        src (.toAbsolutePath (.toPath (.getAbsoluteFile (java.io.File. "src"))))
        running (atom true)]
    ;; Register absolute directories: the watchable is handed back to the
    ;; event handler, and only an absolute one can be relativized reliably.
    (doseq [dir (file-seq (.toFile src))
            :when (.isDirectory dir)]
      (.register (.toPath dir) watcher
                 (into-array [StandardWatchEventKinds/ENTRY_MODIFY])))
    (log/log "dev: watching src/ for changes")
    {:running running
     :future (future
               (while @running
                 (when-let [key (.poll watcher 500 java.util.concurrent.TimeUnit/MILLISECONDS)]
                   (doseq [event (.pollEvents key)]
                     (let [dir (.watchable key)
                           full (.toAbsolutePath (.resolve ^Path dir (str (.context event))))]
                       (when-let [ns-sym (ns-for-change src full)]
                         (reload! ns-sym))))
                   (.reset key)))
               (.close watcher)
               (log/log "dev: watcher stopped"))}))

(defn stop-watcher! [{:keys [running]}]
  (reset! running false))