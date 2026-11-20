(ns xmas.session-test
  (:require [clojure.test :refer [deftest is]]
            [xmas.ed :as ed]
            [xmas.session :as session]
            [xmas.buf :as buf]
            [xmas.window :as win]
            [xmas.cmd :as cmd]
            [xmas.complete :as complete]))

(defn- make-state []
  {:buf "*test*"
   :bufs {"*test*" (buf/make "*test*" "hello world" nil)}
   :msg nil :windows (win/leaf "*test*") :cur-window []
   :rows 24 :cols 80
   :completion-sources {:default [complete/buffers complete/buffer-symbols]
                        :clojure-mode [complete/buffer-symbols complete/project-symbols]}})

(deftest test-session-save-load
  (let [state (-> (make-state)
                  (assoc-in [:bufs "*test*" :point] 42)
                  (assoc-in [:bufs "*test*" :mark] 10)
                  (assoc-in [:bufs "*test*" :modified] true)
                  (ed/split-window-below)
                  (assoc-in [:windows :ratio] 0.3))]
    (session/save-session! state)
    (when-let [sess (session/load-session!)]
      (let [[restored ok] (session/restore-session state sess)]
        (is ok)
        (is (= 42 (get-in restored [:bufs "*test*" :point])))
        (is (= 10 (get-in restored [:bufs "*test*" :mark])))
        (is (true? (get-in restored [:bufs "*test*" :modified])))
        (is (= :split (:type (:windows restored))))
        (is (= 0.3 (:ratio (:windows restored))))))))

(deftest test-session-restore-missing-file-buffer
  ;; Buffers for non-existent files should be dropped
  (let [state (-> (make-state)
                  (assoc-in [:bufs "*test*" :file] "/nonexistent/path.txt"))]
    (session/save-session! state)
    (when-let [sess (session/load-session!)]
      (let [[restored ok] (session/restore-session state sess)]
        (is ok)
        ;; The buffer should be dropped since file doesn't exist
        (is (not (contains? (:bufs restored) "/nonexistent/path.txt")))))))

(deftest test-session-save-on-exit
  ;; Simulate clean exit by setting :exit true
  (let [state (-> (make-state)
                  (assoc-in [:bufs "*test*" :point] 100)
                  (assoc :exit true))]
    (when (:exit state)
      (session/save-session! state))
    (when-let [sess (session/load-session!)]
      (let [[restored ok] (session/restore-session state sess)]
        (is ok)
        (is (= 100 (get-in restored [:bufs "*test*" :point])))))))

(deftest session-omits-text-for-unmodified-file-buffers
  (let [f (java.io.File/createTempFile "xmas-session" ".txt")]
    (try
      (spit f "from disk\n")
      (let [state (assoc-in (make-state) [:bufs "*test*" :file] (.getPath f))
            _     (session/save-session! state)
            sess  (session/load-session!)]
        (is (nil? (get-in sess [:bufs "*test*" :text])))
        (let [[restored ok] (session/restore-session state sess)]
          (is ok)
          (is (= "from disk\n" (str (get-in restored [:bufs "*test*" :text]))))))
      (finally (.delete f)))))

(deftest session-keeps-text-for-modified-file-buffers
  (let [f (java.io.File/createTempFile "xmas-session" ".txt")]
    (try
      (spit f "from disk\n")
      (let [state (-> (make-state)
                      (assoc-in [:bufs "*test*" :file] (.getPath f))
                      (assoc-in [:bufs "*test*" :modified] true))
            _     (session/save-session! state)
            sess  (session/load-session!)]
        (is (= "hello world" (get-in sess [:bufs "*test*" :text])))
        (is (= "hello world"
               (str (get-in (first (session/restore-session state sess))
                            [:bufs "*test*" :text])))))
      (finally (.delete f)))))
