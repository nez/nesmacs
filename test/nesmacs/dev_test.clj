(ns nesmacs.dev-test
  (:require [clojure.test :refer [deftest is testing]]
            [nesmacs.dev :as dev])
  (:import [java.nio.file Paths]))

(defn- path [s] (Paths/get ^String s (make-array String 0)))

(deftest ns-for-file-maps-source-paths
  (is (= 'nesmacs.ed (dev/ns-for-file "src/nesmacs/ed.clj")))
  (is (= 'nesmacs.dev-test (dev/ns-for-file "src/nesmacs/dev_test.clj")))
  (is (= 'nesmacs.foo (dev/ns-for-file "nesmacs/foo.clj")) "already relative to src")
  (testing "only .clj sources are reloaded"
    (is (nil? (dev/ns-for-file "src/nesmacs/query.cljc")))
    (is (nil? (dev/ns-for-file "README.md")))))

(deftest ns-for-change-relativizes-against-the-src-root
  (let [root (path "/proj/src")]
    (is (= 'nesmacs.ed (dev/ns-for-change root (path "/proj/src/nesmacs/ed.clj"))))
    (is (= 'nesmacs.dev-test (dev/ns-for-change root (path "/proj/src/nesmacs/dev_test.clj"))))
    (is (nil? (dev/ns-for-change root (path "/proj/other/ed.clj"))))
    (is (nil? (dev/ns-for-change root (path "/proj/src/nesmacs/ed.cljc"))))))

(deftest a-watch-event-on-a-subdirectory-resolves-to-the-real-namespace
  ;; Regression: relative watchables produced `src.nesmacs.foo`, so `require`
  ;; failed with "Could not locate src/nesmacs/foo.clj on classpath".
  (let [root (path "/home/nex/nesmacs/src")
        watchable (path "/home/nex/nesmacs/src/nesmacs")]
    (is (= 'nesmacs.lsp (dev/ns-for-change root (.resolve ^java.nio.file.Path watchable "lsp.clj"))))))