(ns xmas.gitdiff
  "Git diff buffer: a job buffer showing the diff for a path. Refresh with g,
   jump to file with RET on the file header (if available)."
  (:require [xmas.job :as job]))

(defn- diff-name [path staged?]
  (str "*Git Diff " (if staged? "(staged) " "") path "*"))

(defn open [s dir path staged?]
  (let [name (diff-name path staged?)]
    (job/start s {:argv ["git" "diff" (when staged? "--cached") "--" path]
                  :dir dir
                  :buf-name name
                  :mode :gitdiff-mode})))