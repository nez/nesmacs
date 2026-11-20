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
     :format-entry fmt}))

(defn make-buffer [dir]
  (listbuf/make (spec dir)))

(defn open [s dir]
  (let [canonical (str (java.io.File. dir))]
    (-> s
        (assoc-in [:bufs gitstatus-name] (make-buffer canonical))
        (cmd/set-cur-buffer gitstatus-name))))

(defn- entry-at-point [s]
  (listbuf/entry-at-point s))

(defn- cur-repo-root [s]
  (git/repo-root (str (java.io.File. (:file (cmd/cur s))))))

(defn refresh [s]
  (let [b (cmd/cur s)
        dir (str (java.io.File. (:file b)))]
    (if (git/is-repo? dir)
      (listbuf/refresh s gitstatus-name (spec dir))
      (-> s (assoc-in [:bufs gitstatus-name] (listbuf/make (spec dir)))
          (cmd/msg "Not a git repository")))))

(defn stage [s]
  (if-let [e (entry-at-point s)]
    (let [dir (cur-repo-root s)]
      (when dir
        (git/add dir (:path e))
        (-> s refresh (cmd/msg (str "Staged " (:path e))))))
    s))

(defn unstage [s]
  (if-let [e (entry-at-point s)]
    (let [dir (cur-repo-root s)]
      (when dir
        (git/reset dir (:path e))
        (-> s refresh (cmd/msg (str "Unstaged " (:path e))))))
    s))

(defn diff-at-point [s]
  (if-let [e (entry-at-point s)]
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