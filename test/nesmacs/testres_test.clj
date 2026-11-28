(ns nesmacs.testres-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [nesmacs.api :as api]
            [nesmacs.buf :as buf]
            [nesmacs.cmd :as cmd]
            [nesmacs.job :as job]
            [nesmacs.testres :as testres]
            [nesmacs.testutil :as tu]
            [nesmacs.window :as win]))

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
  (let [dir (str (System/getProperty "java.io.tmpdir") "/nesmacs-default-cmd-"
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

(deftest clojure-test-basename-resolves-only-to-a-unique-file
  (let [root (tu/temp-dir "nesmacs-test-source")
        source "test/demo/app_test.clj"
        parsed (testres/parse "FAIL in (adds) (app_test.clj:12)\nRan 1 tests\n1 failures, 0 errors.\n")
        entry #(-> (testres/show-results {} parsed (.getPath root))
                   (get-in [:bufs "*Test Results*" :listbuf-entries 0]))]
    (try
      (doseq [path [source "other/demo/app_test.clj"]]
        (let [file (java.io.File. root path)]
          (.mkdirs (.getParentFile file))
          (spit file "(ns demo.app-test)")
          (when (= path source)
            (is (= {:file source :line 12} (select-keys (entry) [:file :line]))))))
      (is (nil? (:file (entry))))
      (is (= "Multiple project files match app_test.clj" (:location-error (entry))))
      (finally (tu/delete-tree root)))))

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

(deftest a-passing-run-with-an-unknown-format-is-not-reported-as-unparsed
  ;; A runner that prints "All good" used to look like a parser failure.
  (let [ed (atom {:bufs {"*Tests*" (assoc (buf/make "*Tests*" "All good\n" nil)
                                          :test-report? true
                                          :job-status :done :job-exit 0)}})]
    (testres/tick! ed)
    (is (str/includes? (:msg @ed) "passed"))
    (is (nil? (get-in @ed [:bufs "*Test Results*"])))))

(deftest a-closed-test-job-cannot-publish-a-report
  (let [editor (atom {:bufs {"*Tests*" (assoc (buf/make "*Tests*" "ok 1 - passing\n" nil)
                                              :test-report? true :job-status :done)}})
        parse testres/parse]
    (with-redefs [testres/parse (fn [output]
                                 (swap! editor update :bufs dissoc "*Tests*")
                                 (parse output))]
      (testres/tick! editor))
    (is (= {:bufs {}} @editor))))

(deftest rerunning-the-test-job-builds-a-fresh-report
  (let [name "*Tests*"
        editor (atom (testres/run {:buf "*scratch*"
                                   :bufs {"*scratch*" (buf/make "*scratch*" "" nil)}
                                   :windows (win/leaf "*scratch*") :cur-window []}
                                  "printf 'not ok 1 - failing test\\n'"))
        finish! (fn []
                  (api/run-effects! editor)
                  (loop [remaining 500]
                    (when (and (pos? remaining)
                               (= :running (get-in @editor [:bufs name :job-status])))
                      (Thread/sleep 10)
                      (swap! editor job/apply-drain (job/drain! (:bufs @editor)))
                      (recur (dec remaining))))
                  (testres/tick! editor))]
    (finish!)
    (let [first-report (cmd/buf @editor "*Test Results*")]
      (is (some? first-report))
      (swap! editor #(-> % (cmd/set-cur-buffer name) job/rerun))
      (is (nil? (get-in @editor [:bufs name :test-parsed?])))
      (finish!)
      (is (= "1 failed / 1 tests" (:msg @editor)))
      (is (= "*Test Results*" (:buf @editor)))
      (is (not (identical? first-report (cmd/buf @editor "*Test Results*")))))))
