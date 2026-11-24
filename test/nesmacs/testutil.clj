(ns nesmacs.testutil
  "Temp directories for tests that touch the filesystem. Deletion is recursive:
   a fixture that removes only direct children leaves nested files behind and
   the delete of the directory then fails silently."
  (:import [java.io File]
           [java.nio.file Files]))

(defn temp-dir
  ([] (temp-dir "nesmacs-test"))
  ([prefix]
   (doto (File/createTempFile prefix "")
     (.delete)
     (.mkdirs))))

(defn delete-tree
  "Delete `f` recursively without following symbolic links. A fixture that
   links to a shared install must not wipe that install on cleanup."
  [^File f]
  (when (and (.isDirectory f)
             (not (Files/isSymbolicLink (.toPath f))))
    (doseq [^File c (.listFiles f)] (delete-tree c)))
  (.delete f))