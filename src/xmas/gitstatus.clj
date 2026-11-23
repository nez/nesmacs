(ns xmas.gitstatus
  "Git status buffer built on listbuf. Shows staged/unstaged/untracked files
   with single-key staging (s), unstaging (u), diff (d), commit (c), log (l).
   Refresh with g."
  (:require [xmas.cmd :as cmd]
            [xmas.git :as git]
            [xmas.gitdiff :as gitdiff]
            [xmas.gitlog :as gitlog]
            [xmas.gitcommit :as gitcommit]
            [xmas.listbuf :as listbuf]))

(def ^:private gitstatus-name "*Git Status*")

(defn- fmt [e]
  (let [mark (case (:status e)
               :staged "●"
               :unstaged "○"
               :untracked "?")
        path (:path e)]
    (format " %s %s" mark path)))

(defn- spec [dir]
  (let [entries (git/status dir)]
    {:name         gitstatus-name
     :mode         :gitstatus-mode
     :header       (str "Git: " (or (git/branch-name dir) "no repo"))
     :entries      entries
     :format-entry fmt
     :extras       {:git-dir dir}}))

(defn open [s dir]
  (listbuf/open s gitstatus-name (spec dir)))

(defn- cur-repo-root [s]
  (git/repo-root (git/current-dir (cmd/cur s))))

(defn refresh [s]
  (let [b (cmd/cur s)
        dir (git/current-dir b)]
    (if (git/is-repo? dir)
      (listbuf/refresh s gitstatus-name (spec dir))
      (-> s (assoc-in [:bufs gitstatus-name] (listbuf/make (spec dir)))
          (cmd/msg "Not a git repository")))))

(defn- alter-index
  "Run `git-fn` on the entry at point, then refresh and report `label`."
  [s git-fn label]
  (if-let [e (listbuf/entry-at-point s)]
    (let [dir (cur-repo-root s)]
      (when dir
        (git-fn dir (:path e))
        (-> s refresh (cmd/msg (str label " " (:path e))))))
    s))

(defn stage   [s] (alter-index s git/add   "Staged"))
(defn unstage [s] (alter-index s git/reset "Unstaged"))

(defn diff-at-point [s]
  (if-let [e (listbuf/entry-at-point s)]
    (let [dir (cur-repo-root s)
          staged? (= :staged (:status e))]
      (gitdiff/open s dir (:path e) staged?))
    s))

(defn commit-cmd [s]
  (let [dir (cur-repo-root s)]
    (when dir
      (gitcommit/open s dir))))

(defn log-cmd [s]
  (let [dir (cur-repo-root s)]
    (when dir (gitlog/open s dir))))