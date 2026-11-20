(ns xmas.job-test
  (:require [clojure.test :refer [deftest is]]
            [xmas.buf :as buf]
            [xmas.job :as job]
            [xmas.window :as win]))

(defn- make-state []
  {:buf "*test*"
   :bufs {"*test*" (buf/make "*test*" "" nil)}
   :msg nil :windows (win/leaf "*test*") :cur-window []})

(defn- pump-until-done [s name]
  (loop [s s i 0]
    (if (or (= :done (get-in s [:bufs name :job-status])) (> i 500))
      s
      (do (Thread/sleep 10) (recur (job/pump! s) (inc i))))))

(deftest streams-output-into-a-buffer
  (let [name "*job*"
        s (job/start (make-state) {:argv ["sh" "-c" "printf 'hi\\nthere\\n'"]
                                   :buf-name name})
        s (pump-until-done s name)]
    (is (= "hi\nthere\n" (str (get-in s [:bufs name :text]))))
    (is (= :done (get-in s [:bufs name :job-status])))
    (is (nil? (get-in s [:bufs name :job-proc])))
    (is (:read-only (get-in s [:bufs name])))
    (is (= name (:buf s)))))

(deftest nonzero-exit-is-visible
  (let [name "*job*"
        s (pump-until-done (job/start (make-state) {:argv ["sh" "-c" "exit 2"]
                                                    :buf-name name})
                           name)]
    (is (clojure.string/includes? (str (get-in s [:bufs name :text])) "[exit 2]"))))

(deftest kill-stops-and-marks-the-job
  (let [name "*job*"
        s (job/start (make-state) {:argv ["sleep" "30"] :buf-name name})
        s (job/kill s)]
    (is (= :killed (get-in s [:bufs name :job-status])))
    (is (= "Killed process" (:msg s)))
    (is (nil? (get-in s [:bufs name :job-proc])))
    ;; A later pump must not resurrect the killed job.
    (is (= :killed (get-in (job/pump! s) [:bufs name :job-status])))))

(deftest kill-without-process-says-so
  (is (= "No running process" (:msg (job/kill (make-state))))))

(deftest rerun-restarts-the-same-command
  (let [name "*job*"
        s1 (job/start (make-state) {:argv ["sh" "-c" "printf 'one\\n'"] :buf-name name})
        s1 (pump-until-done s1 name)
        s2 (job/rerun s1)
        s2 (pump-until-done s2 name)]
    (is (= "one\n" (str (get-in s2 [:bufs name :text]))))
    (is (= :done (get-in s2 [:bufs name :job-status])))))

(deftest rerun-non-job-is-a-noop
  (is (= "Not a job buffer" (:msg (job/rerun (make-state))))))

(deftest output-is-truncated-at-the-cap
  (let [b (#'job/append-text (get-in (make-state) [:bufs "*test*"])
                             (apply str (repeat 2000001 "x")))]
    (is (:job-truncated b))
    (is (clojure.string/includes? (str (:text b)) "[output truncated]"))
    (is (< (count (:text b)) 2000100))))