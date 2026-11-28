(ns nesmacs-example.project-notes
  (:require [clojure.java.io :as io]
            [nesmacs.api :as api]
            [nesmacs.cmd :as cmd]
            [nesmacs.ed :as ed]
            [nesmacs.project :as project]))

(def ^:private notes-file (atom "NOTES.md"))

(defn project-notes
  "Open the configured notes file for the current project."
  [s]
  (let [root (or (project/root-for (:file (cmd/cur s))) (System/getProperty "user.dir"))
        base (-> (io/file root) .getCanonicalFile .toPath)
        target (-> (io/file root @notes-file) .getCanonicalFile .toPath)]
    (if (and (.startsWith target base) (not= target base))
      (ed/find-file s (str target))
      (cmd/msg s "Choose a notes file inside the project"))))

(defn start! [{:keys [file]}]
  (reset! notes-file file)
  (api/register-command! #'project-notes))
