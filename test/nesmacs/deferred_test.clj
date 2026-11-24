(ns nesmacs.deferred-test
  (:require [clojure.test :refer [deftest is]]
            [nesmacs.deferred :as deferred]
            [nesmacs.window :as win]))

(defn- state [job-buf]
  {:bufs {"*job*" job-buf "*prev*" {:name "*prev*" :text "" :point 0}}
   :buf "*job*"
   :windows (win/leaf "*job*")
   :cur-window []})

(deftest a-finished-job-becomes-a-list-buffer
  (let [job {:name "*job*" :text "alpha\nbeta\n" :job-status :done
             :deferred (fn [out]
                         ["*list*" {:name "*list*" :mode :nav-mode
                                    :header (str "got " (count (re-seq #"\n" out))
                                                 " newlines")
                                    :entries [] :format-entry str}])
             :deferred-return "*prev*"}
        editor (atom (state job))]
    (deferred/tick! editor)
    (let [s @editor]
      (is (contains? (:bufs s) "*list*") "listing built")
      (is (= "*list*" (:buf s)) "listing shown")
      (is (not (contains? (:bufs s) "*job*")) "raw job buffer dropped"))))

(deftest a-failed-build-shows-the-message-and-keeps-the-previous-buffer
  (let [job {:name "*job*" :text "" :job-status :done
             :deferred (fn [_] "nothing to show")
             :deferred-return "*prev*"}
        editor (atom (state job))]
    (deferred/tick! editor)
    (let [s @editor]
      (is (= "nothing to show" (:msg s)))
      (is (= "*prev*" (:buf s)))
      (is (not (contains? (:bufs s) "*job*"))))))

(deftest a-finishing-job-does-not-steal-an-open-minibuffer
  ;; A prompt owns keys while it is open; installing the listing must not move
  ;; :buf away from the mini buffer mid-typing.
  (let [job {:name "*job*" :text "x\n" :job-status :done
             :deferred (fn [_] ["*list*" {:name "*list*" :mode :nav-mode
                                          :header "" :entries [] :format-entry str}])
             :deferred-return "*prev*"}
        s (assoc (state job) :buf " *mini*" :mini {:prompt "M-x: " :input "foo"})
        editor (atom s)]
    (deferred/tick! editor)
    (let [s @editor]
      (is (contains? (:bufs s) "*list*") "listing exists")
      (is (= " *mini*" (:buf s)) "focus stays in the prompt"))))

(deftest a-running-job-is-left-alone
  (let [job {:name "*job*" :text "partial" :job-status :running
             :deferred (fn [_] ["*list*" {:name "*list*" :header "" :entries []
                                          :format-entry str}])}
        editor (atom (state job))]
    (deferred/tick! editor)
    (is (contains? (:bufs @editor) "*job*"))))