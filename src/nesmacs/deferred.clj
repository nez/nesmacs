(ns nesmacs.deferred
  "Run a process as a job and turn its output into a list buffer when it
   finishes. Keeps slow read-only panels (blame, hunks, forge) off the editor
   thread: opening one starts the command and returns immediately.

   `build` receives the job's output text and returns `[buffer-name spec]` for
   `nesmacs.listbuf/open`, or a message string to show instead."
  (:require [nesmacs.cmd :as cmd]
            [nesmacs.job :as job]
            [nesmacs.listbuf :as listbuf]))

(defn- finish [build return-to b]
  (let [result (try (build (str (:text b)))
                    (catch Exception e (str "Error: " (.getMessage e))))]
    (fn [s name]
      (let [s (cmd/remove-buffer s name return-to)]
        (cond
          (string? result) (cmd/msg s result)
          (vector? result) (let [[buf-name spec] result] (listbuf/open s buf-name spec))
          :else s)))))

(defn start
  "Start a job whose finished output is turned into a list buffer or message."
  [s {:keys [argv dir job-name build]}]
  (job/start s {:argv argv :dir dir :buf-name job-name
                :on-finish (partial finish build (:buf s))}))
