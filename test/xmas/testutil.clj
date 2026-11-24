(ns xmas.testutil
  "Temp directories for tests that touch the filesystem. Deletion is recursive:
   a fixture that removes only direct children leaves nested files behind and
   the delete of the directory then fails silently."
  (:import [java.io File]))

(defn temp-dir
  ([] (temp-dir "xmas-test"))
  ([prefix]
   (doto (File/createTempFile prefix "")
     (.delete)
     (.mkdirs))))

(defn delete-tree [^File f]
  (doseq [^File c (reverse (file-seq f))] (.delete c)))