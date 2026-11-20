(ns xmas.gitlog
  "Git log buffer: listbuf of recent commits. RET shows the commit diff in a
   job buffer. Refresh with g."
  (:require [xmas.cmd :as cmd]
            [xmas.git :as git]
            [xmas.job :as job]
            [xmas.listbuf :as listbuf]))

(def ^:private gitlog-name "*Git Log*")

(defn- fmt [e]
  (format "%s %s  %s %s"
          (:abbrev e) (:date e) (:author e) (:subject e)))

(defn- spec [dir]
  {:name         gitlog-name
   :mode         :gitlog-mode
   :header       "Git log (g to refresh)"
   :entries      (git/log dir 200)
   :format-entry fmt
   :extras       {:git-dir dir}})

(defn open [s dir]
  (listbuf/open s gitlog-name (spec dir)))

(defn show-commit [s]
  (if-let [e (listbuf/entry-at-point s)]
    (let [dir (git/repo-root (git/current-dir (cmd/cur s)))]
      (when dir
        (job/start s {:argv ["git" "show" (:hash e)]
                      :dir dir
                      :buf-name (str "*Git Show " (:abbrev e) "*")
                      :mode :gitdiff-mode})))
    s))

(defn refresh [s]
  (let [b (cmd/cur s)
        dir (git/current-dir b)]
    (if (git/is-repo? dir)
      (listbuf/refresh s gitlog-name (spec dir))
      (-> s (assoc-in [:bufs gitlog-name] (listbuf/make (spec dir)))
          (cmd/msg "Not a git repository")))))