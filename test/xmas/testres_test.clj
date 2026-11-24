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
(deftest clojure-test-runner-output-is-parsed
  ;; clojure -M:test prints human text, not TAP or JUnit; the default command
  ;; still has to yield jumpable failures.
  (let [out (str "Testing a.b-test\n"
                 "FAIL in (adds) (src/a/b_test.clj:12)\n"
                 "expected: (= 3 (add 1 1))\n"
                 "ERROR in (boom) (src/a/c_test.clj:40)\n"
                 "Ran 7 tests containing 9 assertions.\n"
                 "1 failures, 1 errors.\n")
        p (testres/parse out)]
    (is (= :text (:kind p)))
    (is (= 7 (:total p)))
    (is (= 2 (:failed p)))
    (is (= [{:name "adds" :file "src/a/b_test.clj" :line 12}
            {:name "boom" :file "src/a/c_test.clj" :line 40}]
           (mapv #(select-keys % [:name :file :line])
                 (testres/failure-entries p))))))

(deftest pytest-output-is-parsed
  (let [out (str "test_math.py:5: AssertionError: assert 3 == 4\n"
                 "1 failed, 2 passed in 0.03s\n")
        p (testres/parse out)]
    (is (= 3 (:total p)))
    (is (= 1 (:failed p)))
    (is (= {:file "test_math.py" :line 5}
           (select-keys (first (testres/failure-entries p)) [:file :line])))))

(deftest clojure-summary-without-failures
  (let [p (testres/parse "Ran 578 tests containing 6299 assertions.\n0 failures, 0 errors.\n")]
    (is (= 578 (:total p)))
    (is (zero? (:failed p)))))

(deftest generic-fallback-finds-file-line-failures
  ;; A runner with a proprietary format still names the failing location; that
  ;; is enough for `run-tests` to stay useful.
  (let [p (testres/parse (str "Running suite\n"
                              "FAILED tests/test_x.py:12: AssertionError: boom\n"
                              "1 failed, 4 passed in 0.2s\n"))
        e (first (:tests p))]
    (is (= :generic (:kind p)))
    (is (= 1 (:failed p)))
    (is (= 5 (:total p)))
    (is (= {:file "tests/test_x.py" :line 12} (select-keys e [:file :line])))))

(deftest generic-fallback-does-not-duplicate-one-failure
  (let [p (testres/parse (str "FAILED lib/a.rb:3: expected true\n"
                              "lib/a.rb:3:in `block': assertion failed\n"))]
    (is (= 1 (count (:tests p))))))

(deftest a-known-format-with-jumpable-entries-wins
  (let [p (testres/parse (str "Ran 1 tests\n"
                              "FAIL in (x) (src/a.clj:9)\n"
                              "expected\n"
                              "0 failures, 1 error\n"))]
    (is (= :text (:kind p)))
    (is (= "x" (:name (first (:tests p)))) "the known format is not replaced")))
