(ns nesmacs.gitcommit
  "Git commit buffer: edit a message, C-c C-c to commit, C-c C-k to abort."
  (:require [clojure.string :as str]
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

(defn commit-at-point [s]
  (let [b (cmd/cur s)
        dir (:gitcommit-dir b)
        message (str/trim (str (:text b)))]
    (cond
      (:git-commit-request s) (cmd/msg s "Commit in progress")
      (nil? dir) (cmd/msg s "Open a commit buffer from Git status first")
      (str/blank? message) (cmd/msg s "Empty commit message")
      :else (assoc s :git-commit-request {:dir dir :message message
                                         :on-success (:gitcommit-on-success b)}))))

(defn tick!
  "Run a requested commit once on the editor loop, outside retriable swaps."
  [editor]
  (when-let [{:keys [dir message on-success]} (:git-commit-request @editor)]
    (let [out (git/commit dir message)]
      (swap! editor
             (fn [s]
               (let [s (dissoc s :git-commit-request)]
                 (if (str/starts-with? out "Error:")
                   (cmd/msg s (first (str/split-lines out)))
                   (-> (cond-> (close s) on-success on-success)
                       (cmd/msg (or (first (str/split-lines out)) "Commit created"))))))))))

(defn abort [s]
  (if (:git-commit-request s)
    (cmd/msg s "Commit in progress")
    (cmd/msg (close s) "Commit aborted")))

(def gitcommit-keymap
  {[:ctrl \c] {[:ctrl \c] commit-at-point
               [:ctrl \k] abort}
   [:ctrl \g] abort})
