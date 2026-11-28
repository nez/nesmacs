(ns nesmacs.job-test
  (:require [clojure.test :refer [deftest is]]
            [nesmacs.api :as api]
            [nesmacs.buf :as buf]
            [nesmacs.job :as job]
            [nesmacs.proc :as proc]
            [nesmacs.window :as win]))

(defn- make-state []
  {:buf "*test*"
   :bufs {"*test*" (buf/make "*test*" "" nil)}
   :msg nil :windows (win/leaf "*test*") :cur-window []})

(defn- pump-until-done [s name]
  (let [editor (atom s)]
    (api/run-effects! editor)
    (loop [s @editor i 0]
      (if (or (= :done (get-in s [:bufs name :job-status])) (> i 500))
        s
        (do (Thread/sleep 10) (recur (job/pump! s) (inc i)))))))

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
        state (job/start (make-state) {:argv ["sleep" "30"] :buf-name name})
        editor (atom state)]
    (is (nil? (get-in state [:bufs name :job-proc])))
    (api/run-effects! editor)
    (let [id (get-in @editor [:bufs name :job-proc])
          s (job/kill @editor)]
      (is (some? (proc/by-id id)))
      (is (= :killed (get-in s [:bufs name :job-status])))
      (is (= "Killed process" (:msg s)))
      (is (nil? (get-in s [:bufs name :job-proc])))
      (reset! editor s)
      (api/run-effects! editor)
      (is (nil? (proc/by-id id)))
      (is (= :killed (get-in (job/pump! @editor) [:bufs name :job-status]))))))

(deftest a-retried-swap-launches-the-job-once
  (let [editor (atom (make-state))
        attempts (atom 0)
        starts (atom 0)]
    (with-redefs [proc/start (fn [_] (swap! starts inc) {:id 1})]
      (swap! editor (fn [state]
                      (when (= 1 (swap! attempts inc))
                        (swap! editor assoc :concurrent-edit true))
                      (job/start state {:argv ["job"] :buf-name "*job*"})))
      (is (= 2 @attempts))
      (is (zero? @starts))
      (api/run-effects! editor)
      (is (= 1 @starts))
      (is (= 1 (get-in @editor [:bufs "*job*" :job-proc]))))))

(deftest queued-job-launches-only-for-the-current-run
  (let [name "*job*"
        editor (atom (job/start (make-state) {:argv ["old"] :buf-name name}))
        starts (atom [])
        killed (atom [])]
    (with-redefs [proc/start (fn [{:keys [argv]}]
                              (swap! starts conj argv)
                              (swap! editor update :bufs dissoc name)
                              {:id 1})
                  proc/kill! #(swap! killed conj (:id %))]
      (swap! editor job/start {:argv ["new"] :buf-name name})
      (is (empty? @starts))
      (api/run-effects! editor)
      (is (= [["new"]] @starts))
      (is (= [1] @killed))
      (is (nil? (get-in @editor [:bufs name]))))))


(deftest kill-without-process-says-so
  (is (= "No running process" (:msg (job/kill (make-state))))))

(deftest rerun-restarts-the-same-command
  (let [name "*job*"
        s1 (job/start (make-state) {:argv ["sh" "-c" "printf 'one\\n'"] :buf-name name
                                   :mode :nav-mode :follow-output? false
                                   :buffer-fields {:test-root "/project" :test-report? true}})
        s1 (pump-until-done s1 name)
        s2 (job/rerun (assoc-in s1 [:bufs name :test-parsed?] true))
        s2 (pump-until-done s2 name)]
    (is (= {:mode :nav-mode :job-follow-output? false
            :test-root "/project" :test-report? true}
           (select-keys (get-in s2 [:bufs name])
                        [:mode :job-follow-output? :test-root :test-report? :test-parsed?])))
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