(ns nesmacs.gitcommit
  "Git commit buffer: edit a message, C-c C-c to commit, C-c C-k to abort."
  (:require [clojure.string :as str]
            [nesmacs.api :as api]
            [nesmacs.buf :as buf]
            [nesmacs.cmd :as cmd]
            [nesmacs.git :as git]))

(def ^:private commit-name "*Git Commit*")

(defn open [s dir & [on-success]]
  (if (:git-commit-request s)
    (cmd/msg s "Commit in progress")
    (-> s
        (assoc-in [:bufs commit-name]
                  (-> (buf/make commit-name "" nil)
                      (assoc :mode :gitcommit-mode
                             :gitcommit-dir dir
                             :gitcommit-return (:buf s)
                             :gitcommit-on-success on-success)))
        (cmd/set-cur-buffer commit-name))))

(defn- close [s]
  (if-let [b (cmd/buf s commit-name)]
    (cmd/remove-buffer s commit-name (:gitcommit-return b))
    s))

(defn- commit! [publish! dir message on-success]
  (try
    (let [out (git/commit dir message)
          summary (first (str/split-lines out))]
      (if (str/starts-with? out "Error:")
        (publish! cmd/msg summary)
        (do
          (publish! #(cmd/msg (close %) (or summary "Commit created")))
          (when on-success (publish! on-success)))))
    (finally (publish! dissoc :git-commit-request))))

(defn commit-at-point [s]
  (let [{:keys [gitcommit-dir gitcommit-on-success text]} (cmd/cur s)
        message (str/trim (str text))]
    (cond
      (:git-commit-request s) (cmd/msg s "Commit in progress")
      (nil? gitcommit-dir) (cmd/msg s "Open a commit buffer from Git status first")
      (str/blank? message) (cmd/msg s "Empty commit message")
      :else (-> s
                (assoc :git-commit-request true)
                (api/after-command #(commit! % gitcommit-dir message gitcommit-on-success))))))

(defn abort [s]
  (if (:git-commit-request s)
    (cmd/msg s "Commit in progress")
    (cmd/msg (close s) "Commit aborted")))

(def gitcommit-keymap
  {[:ctrl \c] {[:ctrl \c] commit-at-point
               [:ctrl \k] abort}
   [:ctrl \g] abort})
