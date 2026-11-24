(ns nesmacs.dap-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [nesmacs.buf :as buf]
            [nesmacs.dap :as dap]
            [nesmacs.lang :as lang]
            [nesmacs.window :as win]))

;; Breakpoints are process-wide state (the editor is one process), so a test
;; that forgets to clean up would leak into the next one.
(use-fixtures :each
  (fn [f]
    (dap/restore-breakpoints! {})
    (try (f) (finally (dap/restore-breakpoints! {})))))

(deftest adapter-argv-expands-port-and-environment
  (let [home (System/getenv "HOME")
        argv (dap/adapter-argv {:argv ["node" "{env:HOME}" "{port}"]} 4711)]
    (is (= "node" (first argv)))
    (is (= home (second argv)))
    (is (= "4711" (last argv)))))

(deftest adapter-argv-is-nil-when-an-expansion-is-missing
  (testing "a TCP placeholder without a port cannot be expanded"
    (is (nil? (dap/adapter-argv {:argv ["node" "server.js" "{port}"]} nil))))
  (testing "an unset environment variable blocks the adapter"
    (is (nil? (dap/adapter-argv
                {:argv ["node" "{env:NESMACS_TEST_NOT_SET_4242}"]} nil)))))

(deftest available?-is-false-without-an-adapter-or-an-executable
  (is (false? (dap/available? (lang/language :clojure)))
      "Clojure has no DAP adapter; nREPL is the debugging interface")
  (is (false? (dap/available? {:id "x" :dap {:argv ["nesmacs-no-such-adapter-xyz"]}})))
  (testing "a tcp adapter is checked by its executable, not its port"
    (is (true? (dap/available? {:id "x" :dap {:transport :tcp
                                              :argv ["sh" "adapter.js" "{port}"]}})))))

(deftest supported-languages-carry-dap-data
  (doseq [id [:python :javascript :typescript :tsx]]
    (let [dap (get-in lang/languages [id :dap])]
      (is (map? dap) (str id))
      (is (vector? (:argv dap)) (str id))
      (is (contains? #{:stdio :tcp} (:transport dap)) (str id))))
  (is (nil? (get-in lang/languages [:clojure :dap]))))

(deftest toggling-breakpoints-keeps-them-sorted-and-unique
  (is (= #{3} (dap/toggle-breakpoint! "/tmp/a.py" 3)))
  (is (= #{3 7} (dap/toggle-breakpoint! "/tmp/a.py" 7)))
  (is (= #{3} (dap/toggle-breakpoint! "/tmp/a.py" 7)) "toggling twice removes it")
  (is (= [3] (vec (dap/breakpoints-for "/tmp/a.py"))))
  (is (= #{} (dap/toggle-breakpoint! "/tmp/a.py" 3)))
  (is (= #{} (dap/breakpoints-for "/tmp/a.py")) "an empty file leaves the map"))

(deftest breakpoints-survive-a-snapshot-round-trip
  (dap/set-breakpoints! "/tmp/a.py" [9 2 9])
  (dap/set-breakpoints! "/tmp/b.ts" [1])
  (let [snapshot (dap/snapshot-breakpoints)]
    (dap/restore-breakpoints! {})
    (is (= {} (dap/snapshot-breakpoints)))
    (dap/restore-breakpoints! snapshot)
    (is (= [2 9] (vec (dap/breakpoints-for "/tmp/a.py"))))))

(deftest restore-ignores-corrupt-breakpoint-data
  (dap/restore-breakpoints! {:ok "/tmp/a.py" :bad 5
                             "/tmp/b.py" [1 "x" 4] :not-a-path [1]})
  (is (= {"/tmp/b.py" #{1 4}} (dap/snapshot-breakpoints))))

(deftest gutter-shows-breakpoints-and-the-stopped-line
  (let [b {:name "a.py" :file "/tmp/a.py"}]
    (is (nil? (dap/gutter-for b nil)) "no breakpoints and no session: no gutter")
    (dap/set-breakpoints! "/tmp/a.py" [2])
    (let [g (dap/gutter-for b nil)]
      (is (= 1 (:width g)))
      (is (= "●" (get-in g [:marks 2 :text])))
      (is (= :breakpoint (get-in g [:marks 2 :face]))))
    (testing "the stopped line wins over a breakpoint on the same line"
      (dap/set-breakpoints! "/tmp/a.py" [2])
      (let [g (dap/gutter-for b {:file "/tmp/a.py" :line 2})]
        (is (= "▶" (get-in g [:marks 2 :text])))
        (is (= :debug-stop (get-in g [:marks 2 :face])))))
    (testing "another buffer has no gutter"
      (is (nil? (dap/gutter-for {:name "b.py" :file "/tmp/b.py"} nil))))))

(deftest tick-installs-gutters-on-buffers-with-breakpoints
  (let [editor (atom {:bufs {"a" (buf/make "a" "x" "/tmp/nesmacs-dap-a.py")
                             "b" (buf/make "b" "y" nil)}
                      :windows (win/leaf "a") :cur-window []})]
    (dap/set-breakpoints! "/tmp/nesmacs-dap-a.py" [0])
    (dap/tick! editor)
    (is (= {:width 1 :marks {0 {:text "●" :face :breakpoint}}}
           (get-in @editor [:bufs "a" :gutter])))
    (is (nil? (get-in @editor [:bufs "b" :gutter])))
    (dap/set-breakpoints! "/tmp/nesmacs-dap-a.py" [])
    (dap/tick! editor)
    (is (nil? (get-in @editor [:bufs "a" :gutter]))
        "removing the last breakpoint clears the gutter")))

(deftest start!-explains-a-missing-adapter-instead-of-failing
  (is (re-find #"No debug adapter"
               (dap/start! {:editor (atom {})
                            :root (System/getProperty "java.io.tmpdir")
                            :file "/tmp/x.clj"
                            :language (lang/language :clojure)}))))

(deftest commands-without-a-session-report-it
  (is (= "No debug session" (dap/continue-run!)))
  (is (= "No debug session" (dap/variables!)))
  (is (= "No debug session" (dap/evaluate! "x")))
  (is (= "debug: off" (dap/status-line)))
  (is (false? (dap/running?))))
(deftest a-stack-frame-carries-the-one-based-line-nav-jump-uses
  ;; DAP reports 1-based lines and nav-jump/goto-line also are 1-based. Storing
  ;; a decremented line made RET on *Debug* land one line above the frame.
  (let [ed (atom {:bufs {}})
        entries (#'dap/build-frames! {:editor ed}
                                      [{:name "main" :line 12
                                        :source {:path "/tmp/nesmacs-frame.py"}}])
        b (get-in @ed [:bufs "*Debug*"])]
    (is (= 12 (:line (first entries))))
    (is (str/includes? (str (:text b)) "/tmp/nesmacs-frame.py:12"))))
