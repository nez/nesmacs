(ns xmas.testres-test
  (:require [clojure.test :refer [deftest is testing]]
            [xmas.testres :as testres]))

(deftest plain-output-is-not-a-test-report
  (is (nil? (testres/parse "hello\nworld\n"))))

(deftest tap-parses-ok-and-not-ok
  (let [p (testres/parse "TAP version 13\n1..3\nok 1 - alpha\nnot ok 2 - beta\nok 3 - gamma\n")]
    (is (= :tap (:kind p)))
    (is (= 3 (:total p)))
    (is (= 1 (:failed p)))
    (is (= ["beta"] (mapv :name (testres/failure-entries p))))))

(deftest junit-parses-testcases-and-failures
  (let [xml (str "<testsuite>"
                 "<testcase classname=\"a.B\" name=\"one\"/>"
                 "<testcase classname=\"a.B\" name=\"two\">"
                 "<failure message=\"boom\">at src/a_b.clj:12</failure>"
                 "</testcase>"
                 "<testcase classname=\"a.C\" name=\"three\">"
                 "<error message=\"kaboom\"/>"
                 "</testcase>"
                 "</testsuite>")
        p (testres/parse xml)]
    (is (= :junit (:kind p)))
    (is (= 3 (:total p)))
    (is (= 2 (:failed p)))
    (testing "the first file:line in the message is used for jumping"
      (is (= {:file "src/a_b.clj" :line 12}
             (select-keys (first (testres/failure-entries p)) [:file :line]))))))

(deftest junit-self-closing-failure-with-message-attr
  (let [xml "<testcase name=\"x\" classname=\"C\"><failure message=\"nope\"/></testcase>"
        e (first (testres/failure-entries (testres/parse xml)))]
    (is (= "nope" (:message e)))))

(deftest default-command-follows-the-project-files
  (let [dir (str (System/getProperty "java.io.tmpdir") "/xmas-default-cmd-"
                 (System/nanoTime))]
    (.mkdirs (java.io.File. dir))
    (try
      (is (= "" (testres/default-command dir)))
      (spit (str dir "/deps.edn") "{}")
      (is (= "clojure -M:test" (testres/default-command dir)))
      (finally (.delete (java.io.File. dir "deps.edn"))
               (.delete (java.io.File. dir))))))