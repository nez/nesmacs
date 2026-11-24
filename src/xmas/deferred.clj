(ns xmas.deferred
  "Run a process as a job and turn its output into a list buffer when it
   finishes. Keeps slow read-only panels (blame, hunks, forge) off the editor
   thread: opening one starts the command and returns immediately.

   `build` receives the job's output text and returns `[buffer-name spec]` for
   `xmas.listbuf/open`, or a message string to show instead."
  (:require [xmas.cmd :as cmd]
            [xmas.job :as job]
            [xmas.listbuf :as listbuf]))

(defn cleanup-completion-cache!
  "Remove completion cache entries for buffer versions that are no longer
   current. Runs from the editor loop."
  [editor comp-cache]
  (let [s @editor
        current-versions (into {} (for [[_ b] (:bufs s)
                                        :when (:file b)]
                                    [(str (:file b)) (or (:version b) 0)]))]
    (swap! comp-cache
           (fn [cache]
             (reduce-kv (fn [acc path {:keys [v pos]}]
                          (if (= v (get current-versions path))
                            (assoc acc path {:v v :pos pos})
                            acc))
                        {} cache)))))

(defn start
  "Start `argv` in `dir` as a job named `job-name`. `build` runs once the job
   exits, on the editor loop."
  [s {:keys [argv dir job-name build]}]
  (-> (job/start s {:argv argv :dir dir :buf-name job-name})
      (update-in [:bufs job-name]
                 assoc :deferred build :deferred-return (:buf (cmd/cur s)))))

(defn tick!
  "Editor-loop step: finish deferred jobs once and clean stale completion cache."
  [editor & [comp-cache]]
  (when comp-cache (cleanup-completion-cache! editor comp-cache))
  (let [s @editor
        done (keep (fn [[name b]]
                     (when (and (:deferred b)
                                (#{:done :failed} (:job-status b))
                                (not (:deferred-done? b)))
                       [name b]))
                   (:bufs s))]
    (doseq [[name b] done]
      (swap! editor update-in [:bufs name] assoc :deferred-done? true)
      (let [out   (str (:text b))
            build (:deferred b)
            res   (try (build out)
                       (catch Exception e (str "Error: " (.getMessage e))))]
        (cond
          (string? res) (swap! editor
                               (fn [st]
                                 (-> st
                                     (cond-> (and (:deferred-return b)
                                                  (= (:buf st) name))
                                       (cmd/set-cur-buffer (:deferred-return b)))
                                     (cmd/msg res))))
          (vector? res) (let [[buf-name spec] res]
                          (swap! editor listbuf/open buf-name spec))
          :else nil)
        ;; The raw job buffer was only a pipe for this command; once the
        ;; listing exists its contents are noise.
        (swap! editor update :bufs dissoc name)))))