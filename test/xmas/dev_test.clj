(ns xmas.dev-test
  (:require [clojure.test :refer [deftest is testing]]
            [xmas.dev :as dev])
  (:import [java.nio.file Paths]))

(defn- path [s] (Paths/get ^String s (make-array String 0)))

(deftest ns-for-file-maps-source-paths
  (is (= 'xmas.ed (dev/ns-for-file "src/xmas/ed.clj")))
  (is (= 'xmas.dev-test (dev/ns-for-file "src/xmas/dev_test.clj")))
  (is (= 'xmas.foo (dev/ns-for-file "xmas/foo.clj")) "already relative to src")
  (testing "only .clj sources are reloaded"
    (is (nil? (dev/ns-for-file "src/xmas/query.cljc")))
    (is (nil? (dev/ns-for-file "README.md")))))

(deftest ns-for-change-relativizes-against-the-src-root
  (let [root (path "/proj/src")]
    (is (= 'xmas.ed (dev/ns-for-change root (path "/proj/src/xmas/ed.clj"))))
    (is (= 'xmas.dev-test (dev/ns-for-change root (path "/proj/src/xmas/dev_test.clj"))))
    (is (nil? (dev/ns-for-change root (path "/proj/other/ed.clj"))))
    (is (nil? (dev/ns-for-change root (path "/proj/src/xmas/ed.cljc"))))))

(deftest a-watch-event-on-a-subdirectory-resolves-to-the-real-namespace
  ;; Regression: relative watchables produced `src.xmas.foo`, so `require`
  ;; failed with "Could not locate src/xmas/foo.clj on classpath".
  (let [root (path "/home/nex/xmas/src")
        watchable (path "/home/nex/xmas/src/xmas")]
    (is (= 'xmas.lsp (dev/ns-for-change root (.resolve ^java.nio.file.Path watchable "lsp.clj"))))))