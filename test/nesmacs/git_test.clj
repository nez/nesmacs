(ns nesmacs.git-test
  (:require [clojure.test :refer [deftest is]]
            [nesmacs.git :as git]
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

(deftest status-parses-the-fields-not-the-record-type
  ;; Porcelain v2 puts XY first and the path last; the old parser used the
  ;; record type as status and XY as the file name.
  (let [entry #'git/status-entry]
    (is (= {:path "src/a b.clj" :status :unstaged :staged false}
           (entry "1 .M N... 100644 100644 100644 abc def src/a b.clj")))
    (is (= {:path "b.clj" :status :staged :staged true}
           (entry "1 M. N... 100644 100644 100644 abc def b.clj")))
    (is (= {:path "new.txt" :status :untracked :staged false}
           (entry "? new.txt")))
    (is (nil? (entry "orig-name.txt")) "a rename's origin arrives as its own chunk")))

(deftest log-survives-an-empty-repository
  (let [dir (tu/temp-dir "nesmacs-empty")]
    (try
      (is (= "" (#'nesmacs.git/run ["git" "init" "-q"] dir)))
      (is (= [] (git/log dir 5)))
      (finally (tu/delete-tree dir)))))
