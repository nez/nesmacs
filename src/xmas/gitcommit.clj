(ns xmas.gitcommit
  "Git commit buffer: edit a message, C-c C-c to commit, C-c C-k to abort."
  (:require [clojure.string :as str]
            [xmas.buf :as buf]
            [xmas.cmd :as cmd]
            [xmas.git :as git]))

(def ^:private commit-name "*Git Commit*")

(defn open [s dir]
  (-> s
      (assoc-in [:bufs commit-name]
                (-> (buf/make commit-name "" nil)
                    (assoc :mode :gitcommit-mode
                           :gitcommit-dir dir)))
      (cmd/set-cur-buffer commit-name)))

(defn commit-at-point [s]
  (let [b (cmd/cur s)
        dir (:gitcommit-dir b)
        msg (str/trim (str (:text b)))]
    (if (str/blank? msg)
      (cmd/msg s "Empty commit message")
      (let [out (git/commit dir msg)]
        (if (str/starts-with? out "Error:")
          (cmd/msg s out)
          (-> s (dissoc :bufs commit-name)
                (cmd/msg out)))))))

(defn abort [s]
  (-> s (dissoc :bufs commit-name)
        (cmd/msg "Commit aborted")))

(def gitcommit-keymap
  {[:ctrl \c] {[:ctrl \c] commit-at-point
               [:ctrl \k] abort}
   [:ctrl \g] abort})