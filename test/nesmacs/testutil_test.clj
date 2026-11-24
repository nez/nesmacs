(ns nesmacs.testutil-test
  (:require [clojure.test :refer [deftest is]]
            [nesmacs.testutil :as tu])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest delete-tree-does-not-follow-symlinks
  ;; The LSP fixture links node_modules/typescript to a shared install; a
  ;; delete that follows the link used to empty the real package.
  (let [root (tu/temp-dir "nesmacs-symlink-test")
        real (File. ^File root "real")
        link (File. ^File root "link")
        keep (File. real "keep.txt")]
    (try
      (.mkdirs real)
      (spit keep "keep")
      (Files/createSymbolicLink (.toPath link) (.toPath real)
                                (make-array FileAttribute 0))
      (tu/delete-tree link)
      (is (.exists keep) "the symlink target is untouched")
      (is (not (.exists link)) "the symlink itself is removed")
      (finally
        (tu/delete-tree root)))))