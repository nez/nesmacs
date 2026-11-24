(ns xmas.complete-test
  (:require [clojure.test :refer [deftest is]]
            [xmas.complete :as complete]))

(deftest an-abandoned-completion-request-is-forgotten
  ;; Every keystroke started a request; the previous one was never removed,
  ;; so a cancelled prompt leaked one candidate vector per keystroke.
  (let [inflight (var-get #'complete/inflight)]
    (reset! inflight {1 {:remaining 1 :results []}})
    (#'complete/tick! (atom {:mini nil}))
    (is (empty? @inflight))))
