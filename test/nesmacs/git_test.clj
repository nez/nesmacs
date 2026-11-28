(ns nesmacs.git-test
  (:require [clojure.test :refer [deftest is]]
            [nesmacs.buf :as buf]
            [nesmacs.cmd :as cmd]
            [nesmacs.ed :as ed]
            [nesmacs.gap :as gap]
            [nesmacs.git :as git]
            [nesmacs.gitdiff :as gitdiff]
            [nesmacs.githunk :as githunk]
            [nesmacs.gitlog :as gitlog]
            [nesmacs.gitstatus :as gitstatus]
            [nesmacs.job :as job]
            [nesmacs.listbuf :as listbuf]
            [nesmacs.proc :as proc]
            [nesmacs.testutil :as tu]))

(deftest current-dir-precedence
  (is (= "/tmp/explicit" (git/current-dir {:git-dir "/tmp/explicit"
                                           :job-dir "/tmp/job"
                                           :file "/tmp/a/file"})))
  (is (= "/tmp/job" (git/current-dir {:job-dir "/tmp/job"
                                      :file "/tmp/a/file"})))
  (is (= "/tmp/a" (git/current-dir {:file "/tmp/a/file"})))
  (is (= (System/getProperty "user.dir") (git/current-dir {}))))
(deftest split-hunks-groups-by-header
  ;; Hunks are staged one at a time, so each patch must carry the file header.
  (let [diff (str "diff --git a/x b/x\n--- a/x\n+++ b/x\n"
                  "@@ -1,2 +1,2 @@\n-a\n+b\n"
                  "@@ -9 +9 @@\n-c\n+d\n")
        hunks (git/parse-hunks diff)]
    (is (= 2 (count hunks)))
    (is (= "@@ -1,2 +1,2 @@" (:label (first hunks))))
    (is (clojure.string/includes? (:patch (second hunks)) "diff --git a/x b/x"))
    (is (clojure.string/includes? (:patch (second hunks)) "-c"))
    (is (not (clojure.string/includes? (:patch (first hunks)) "-c")))))

(deftest refreshed-staged-hunks-can-still-be-unstaged
  (let [dir "/repo"
        path "/repo/file.txt"
        hunk {:index 0 :label "@@ -1 +1 @@" :patch "patch"}
        s {:buf "*Hunks*"
           :bufs {"*Hunks*" (assoc (buf/make "*Hunks*")
                                   :git-dir dir :hunk-path path :hunk-staged? true)}}]
    (with-redefs [git/staged-hunks (fn [root file]
                                    (is (= [dir path] [root file]))
                                    [hunk])
                  git/apply-cached (fn [root patch reverse?]
                                     (is (= [dir "patch" true] [root patch reverse?]))
                                     "")]
      (let [s (githunk/refresh s)
            s (cmd/update-cur s #(assoc % :point (gap/nth-line-start (:text %) 1)))
            result (githunk/unstage s)]
        (is (= "Unstaged hunk 0" (:msg result)))
        (is (= [(assoc hunk :path path :staged true)]
               (:listbuf-entries (cmd/cur result))))))))

(deftest blame-parses-line-porcelain
  (let [dir (tu/temp-dir "nesmacs-blame")
        git (fn [& args]
              (let [pb (doto (ProcessBuilder. (into-array String (cons "git" args)))
                         (.directory dir) (.redirectErrorStream true))
                    p (.start pb)
                    out (slurp (.getInputStream p))]
                (.waitFor p)
                out))]
    (try
      (spit (str dir "/f.txt") "alpha\nbeta\n")
      (git "init" "-q")
      (git "-c" "user.name=Tester" "-c" "user.email=t@example.com" "add" "f.txt")
      (git "-c" "user.name=Tester" "-c" "user.email=t@example.com" "commit" "-qm" "one")
      (let [rows (git/parse-blame (git "blame" "--line-porcelain" "--" "f.txt"))]
        (is (= 2 (count rows)))
        (is (= {:line 1 :author "Tester" :text "alpha"}
               (select-keys (first rows) [:line :author :text])))
        (is (= (:hash (first rows)) (:hash (second rows)))))
      (finally (tu/delete-tree dir)))))

(deftest status-separates-index-and-worktree-changes
  (let [out (clojure.string/join
              (str (char 0))
              ["1 .M N... 100644 100644 100644 abc def src/a b.clj"
               "1 M. N... 100644 100644 100644 abc def b.clj"
               "1 MM N... 100644 100644 100644 abc def mixed.clj"
               "2 RM N... 100644 100644 100644 abc def R100 renamed.clj"
               "? old.clj"
               "u UU N... 100644 100644 100644 100644 abc def ghi conflict.clj"
               "? new.txt" ""])]
    (with-redefs [proc/run (constantly out)]
      (is (= [{:path "src/a b.clj" :status :unstaged :staged false}
              {:path "b.clj" :status :staged :staged true}
              {:path "mixed.clj" :status :staged :staged true}
              {:path "mixed.clj" :status :unstaged :staged false}
              {:path "renamed.clj" :status :staged :staged true}
              {:path "renamed.clj" :status :unstaged :staged false}
              {:path "conflict.clj" :status :staged :staged true}
              {:path "new.txt" :status :untracked :staged false}]
             (git/status-at-root "/repo"))))))

(deftest status-refresh-resolves-root-once
  (let [calls (atom [])
        s {:buf "*Git Status*"
           :bufs {"*Git Status*" (assoc (buf/make "*Git Status*") :git-dir "/repo/src")}}]
    (with-redefs [proc/run (fn [argv dir _]
                            (swap! calls conj [argv dir])
                            (cond
                              (some #{"--show-toplevel"} argv) "/repo\n"
                              (some #{"--abbrev-ref"} argv) "main\n"
                              :else ""))]
      (let [result (gitstatus/refresh s)]
        (is (= [[["git" "rev-parse" "--show-toplevel"] "/repo/src"]
                [["git" "status" "--porcelain=v2" "-z"] "/repo"]
                [["git" "rev-parse" "--abbrev-ref" "HEAD"] "/repo"]]
               @calls))
        (is (= "/repo" (:git-dir (cmd/cur result))))))
    (with-redefs [git/repo-root (constantly nil)]
      (let [result (gitstatus/refresh s)]
        (is (= "/repo/src" (:git-dir (cmd/cur result))))
        (is (= "Not a git repository" (:msg result))))
      (is (nil? (git/status "/repo/src")))
      (is (nil? (git/branch-name "/repo/src"))))))

(deftest git-commands-preserve-state-when-there-is-nothing-to-open
  (let [state (fn [line] {:buf "view" :bufs {"view" (buf/make "view" line nil)}})
        plain (state "no header")
        header (state "diff --git a/f b/f")]
    (with-redefs [git/repo-root (constantly nil)
                  listbuf/entry-at-point (constantly {:hash "abc1234"})]
      (is (= plain (ed/diff-open-at-point plain)))
      (is (= (cmd/msg header "Not a git repository") (ed/diff-open-at-point header)))
      (is (= (cmd/msg plain "Not a git repository") (gitstatus/log-cmd plain)))
      (is (= (cmd/msg plain "Not a git repository") (gitlog/show-commit plain))))))

(deftest status-diff-uses-the-shared-argv
  (with-redefs [job/start (fn [_ task] task)]
    (is (= ["git" "diff" "--cached" "--no-color" "--" "a b"]
           (:argv (gitdiff/open {} "/repo" "a b" true))))
    (is (= ["git" "diff" "--no-color" "--" "a b"]
           (:argv (gitdiff/open {} "/repo" "a b" false))))))

(deftest log-survives-an-empty-repository
  (let [dir (tu/temp-dir "nesmacs-empty")]
    (try
      (is (= "" (#'nesmacs.git/run ["git" "init" "-q"] dir)))
      (is (= [] (git/log dir 5)))
      (finally (tu/delete-tree dir)))))
