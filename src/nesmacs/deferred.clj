(ns nesmacs.deferred
  "Run a process as a job and turn its output into a list buffer when it
   finishes. Keeps slow read-only panels (blame, hunks, forge) off the editor
   thread: opening one starts the command and returns immediately.

   `build` receives the job's output text and returns `[buffer-name spec]` for
   `nesmacs.listbuf/open`, or a message string to show instead."
  (:require [nesmacs.cmd :as cmd]
            [nesmacs.job :as job]
            [nesmacs.listbuf :as listbuf]))

(defn start
  "Start `argv` in `dir` as a job named `job-name`. `build` runs once the job
   exits, on the editor loop."
  [s {:keys [argv dir job-name build]}]
  (job/start s {:argv argv :dir dir :buf-name job-name
                :buffer-fields {:deferred build :deferred-return (:buf s)}}))

(defn- pending? [b]
  (and (:deferred b)
       (#{:done :failed} (:job-status b))))

(defn- build-result [b]
  (try ((:deferred b) (str (:text b)))
       (catch Exception e (str "Error: " (.getMessage e)))))

(defn- publish-result [s name b result]
  (let [s (cmd/remove-buffer s name (:deferred-return b))]
    (cond
      (string? result) (cmd/msg s result)
      (vector? result) (let [[buf-name spec] result] (listbuf/open s buf-name spec))
      :else s)))

(defn tick!
  "Editor-loop step: finish deferred jobs once."
  [editor]
  (job/consume-results! editor pending? build-result publish-result))
