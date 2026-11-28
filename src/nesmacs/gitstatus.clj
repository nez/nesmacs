(ns nesmacs.gitstatus
  "Git status buffer built on listbuf. Shows staged/unstaged/untracked files
   with single-key staging (s), unstaging (u), diff (d), commit (c), log (l).
   Refresh with g."
  (:require [clojure.string :as str]
            [nesmacs.cmd :as cmd]
            [nesmacs.git :as git]
            [nesmacs.gitdiff :as gitdiff]
            [nesmacs.gitlog :as gitlog]
            [nesmacs.gitcommit :as gitcommit]
            [nesmacs.listbuf :as listbuf]))

(def ^:private gitstatus-name "*Git Status*")

(defn- fmt [e]
  (let [mark (case (:status e)
               :staged "●"
               :unstaged "○"
               :untracked "?")
        path (:path e)]
    (format " %s %s" mark path)))

(defn- spec [root]
  (let [entries (git/status-at-root root)]
    {:name         gitstatus-name
     :mode         :gitstatus-mode
     :header       (str "Git: " (or (git/branch-name-at-root root) "no repo")
                        "   RET diff  s stage  u unstage  c commit  g refresh")
     :entries      entries
     :format-entry fmt
     :extras       {:git-dir root}}))

(defn open [s root]
  (listbuf/open s gitstatus-name (spec root)))

(defn- cur-repo-root [s]
  (git/repo-root (git/current-dir (cmd/cur s))))

(defn refresh [s]
  (let [dir (git/current-dir (cmd/cur s))
        root (git/repo-root dir)
        listing (assoc-in (spec root) [:extras :git-dir] (or root dir))]
    (listbuf/reload s gitstatus-name listing (some? root)
                    "Not a git repository")))

(defn- alter-index
  "Run `git-fn` on the entry at point, then refresh and report `label`."
  [s git-fn label]
  (if-let [e (listbuf/entry-at-point s)]
    (if-let [dir (cur-repo-root s)]
      (let [out (git-fn dir (:path e))]
        (if (str/starts-with? out "Error:")
          (cmd/msg s (first (str/split-lines out)))
          (-> s refresh (cmd/msg (str label " " (:path e))))))
      (cmd/msg s "Not a git repository"))
    s))

(defn stage   [s] (alter-index s git/add   "Staged"))
(defn unstage [s] (alter-index s git/reset "Unstaged"))

(defn diff-at-point [s]
  (if-let [e (listbuf/entry-at-point s)]
    (if-let [dir (cur-repo-root s)]
      (gitdiff/open s dir (:path e) (= :staged (:status e)))
      (cmd/msg s "Not a git repository"))
    s))

(defn commit-cmd [s]
  (if-let [dir (cur-repo-root s)]
    (gitcommit/open s dir
      (fn [state]
        (if (= dir (get-in state [:bufs gitstatus-name :git-dir]))
          (listbuf/refresh state gitstatus-name (spec dir))
          state)))
    (cmd/msg s "Not a git repository")))

(defn log-cmd [s]
  (if-let [dir (cur-repo-root s)]
    (gitlog/open s dir)
    (cmd/msg s "Not a git repository")))
