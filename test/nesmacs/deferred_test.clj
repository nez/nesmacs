(ns nesmacs.deferred-test
  (:require [clojure.test :refer [deftest is]]
            [nesmacs.api :as api]
            [nesmacs.cmd :as cmd]
            [nesmacs.deferred :as deferred]
            [nesmacs.gap :as gap]
            [nesmacs.job :as job]
            [nesmacs.proc :as proc]
            [nesmacs.window :as win]))

(defn- with-finish [b]
  (when b
    (assoc b :job-finish (partial @#'deferred/finish (:deferred b) (:deferred-return b)))))

(defn- state [job-buf]
  {:bufs {"*job*" (with-finish job-buf) "*prev*" {:name "*prev*" :text "" :point 0}}
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
    (job/consume-results! editor)
    (let [s @editor]
      (is (contains? (:bufs s) "*list*") "listing built")
      (is (= "*list*" (:buf s)) "listing shown")
      (is (not (contains? (:bufs s) "*job*")) "raw job buffer dropped"))))

(deftest a-failed-build-shows-the-message-and-keeps-the-previous-buffer
  (let [job {:name "*job*" :text "" :job-status :done
             :deferred (fn [_] "nothing to show")
             :deferred-return "*prev*"}
        editor (atom (state job))]
    (job/consume-results! editor)
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
        s (assoc (state job) :buf " *mini*" :mini {:prompt "M-x: " :prev-buf "*job*"})
        editor (atom s)]
    (job/consume-results! editor)
    (let [s @editor]
      (is (contains? (:bufs s) "*list*") "listing exists")
      (is (= " *mini*" (:buf s)) "focus stays in the prompt")
      (is (= "*prev*" (get-in s [:mini :prev-buf])))
      (is (= "*prev*" (get-in s [:windows :buffer]))))))

(deftest a-finished-job-cannot-publish-over-a-rerun
  (let [editor (atom nil)
        replacement (with-finish {:text (gap/of "same output") :job-status :done
                     :deferred (constantly "new result") :deferred-return "*prev*"})
        original (assoc replacement :text (gap/of "same output")
                        :deferred (fn [_]
                                    (swap! editor assoc-in [:bufs "*job*"] replacement)
                                    "old result"))]
    (reset! editor (state original))
    (job/consume-results! editor)
    (is (identical? replacement (get-in @editor [:bufs "*job*"])))
    (is (nil? (:msg @editor)))
    (job/consume-results! editor)
    (is (= "new result" (:msg @editor)))
    (is (not (contains? (:bufs @editor) "*job*")))))

(deftest a-running-job-is-left-alone
  (let [job {:name "*job*" :text "partial" :job-status :running
             :deferred (fn [_] ["*list*" {:name "*list*" :header "" :entries []
                                          :format-entry str}])}
        editor (atom (state job))]
    (job/consume-results! editor)
    (is (contains? (:bufs @editor) "*job*"))))

(deftest rerunning-a-deferred-job-keeps-its-builder-and-return-buffer
  (let [editor (atom (deferred/start (cmd/set-cur-buffer (state nil) "*prev*")
                                     {:argv ["job"] :job-name "*job*" :build str}))]
    (swap! editor job/rerun)
    (with-redefs [proc/start (constantly {:id 1})]
      (api/run-effects! editor))
    (swap! editor job/apply-drain {"*job*" {:text "result" :exit 0 :proc-id 1}})
    (job/consume-results! editor)
    (is (= "result" (:msg @editor)))
    (is (= "*prev*" (:buf @editor)))
    (is (nil? (cmd/buf @editor "*job*")))))
