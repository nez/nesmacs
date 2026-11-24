(ns xmas.project-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [xmas.project :as project])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- tmp-dir []
  (.toFile (Files/createTempDirectory "xmas-proj" (into-array FileAttribute []))))

(defn- write! [root rel content]
  (let [f (io/file root rel)]
    (io/make-parents f)
    (spit f content)
    f))

(defn- rm-rf [f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)] (rm-rf c)))
  (.delete f))

(deftest root-walks-up-to-a-marker
  (let [root (tmp-dir)]
    (try
      (write! root ".git/HEAD" "")
      (write! root "src/a.clj" "")
      (is (= (.getCanonicalPath root)
             (.getCanonicalPath (io/file (project/root (str root "/src/a.clj"))))))
      (finally (rm-rf root)))))

(deftest root-of-an-unmarked-dir-is-nil
  (let [dir (tmp-dir)]
    (try
      (is (nil? (project/root (str dir))))
      (finally (rm-rf dir)))))

(deftest files-honour-the-minimal-ignore-matcher
  (let [root (tmp-dir)]
    (try
      (write! root ".git/HEAD" "")
      (write! root ".gitignore" "*.log\nbuild\n# comment\n")
      (write! root "src/a.clj" "")
      (write! root "src/b.log" "")
      (write! root "build/x.clj" "")
      (write! root "README.md" "")
      (write! root "sub/.gitignore" "tmp.txt\n")
      (write! root "sub/tmp.txt" "")
      (write! root "sub/keep.clj" "")
      (is (= [".gitignore" "README.md" "src/a.clj" "sub/.gitignore" "sub/keep.clj"]
             (project/files root)))
      (finally (rm-rf root)))))

(deftest fuzzy-matches-subsequences-and-ranks-exact-names-first
  (let [files ["src/alpha.clj" "src/beta.clj" "test/alpha_test.clj"]]
    (is (= "src/alpha.clj" (first (project/fuzzy files "alpha.clj"))))
    (is (= #{"src/alpha.clj" "test/alpha_test.clj"}
           (set (project/fuzzy files "alp"))))
    (is (empty? (project/fuzzy files "zzz")))))
(deftest language-manifests-are-project-roots
  ;; LSP and test runs need a root for Python and Node projects that have no
  ;; deps.edn or .git of their own.
  (doseq [manifest ["pyproject.toml" "package.json"]]
    (let [root (tmp-dir)]
      (try
        (write! root manifest "{}")
        (write! root "src/a" "")
        (is (= (.getCanonicalPath root)
               (.getCanonicalPath (io/file (project/root (str root "/src/a")))))
            manifest)
        (finally (rm-rf root))))))
