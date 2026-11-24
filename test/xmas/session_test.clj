(ns xmas.session-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [xmas.ed :as ed]
            [xmas.session :as session]
            [xmas.buf :as buf]
            [xmas.persist :as persist]
            [xmas.window :as win]
            [xmas.cmd :as cmd]
            [xmas.complete :as complete]))

(defn- temp-dir []
  (doto (java.io.File/createTempFile "xmas-session-dir" "")
    (.delete)
    (.mkdirs)))

(defn- delete-tree [^java.io.File f]
  (doseq [c (reverse (file-seq f))] (.delete c)))

;; Never read or write the real ~/.xmas from tests; the journal tests in
;; particular would otherwise trim a running editor's journal.
(use-fixtures :each
  (fn [f]
    (let [dir (temp-dir)]
      (try (binding [persist/*dir* dir] (f))
           (finally (delete-tree dir))))))

(defn- make-state []
  {:buf "*test*"
   :bufs {"*test*" (buf/make "*test*" "hello world" nil)}
   :msg nil :windows (win/leaf "*test*") :cur-window []
   :rows 24 :cols 80
   :completion-sources {:default [complete/buffers complete/buffer-symbols]
                        :clojure-mode [complete/buffer-symbols complete/project-symbols]}})

(deftest test-session-save-load
  (let [state (-> (make-state)
                  (assoc-in [:bufs "*test*" :point] 7)
                  (assoc-in [:bufs "*test*" :mark] 3)
                  (assoc-in [:bufs "*test*" :modified] true)
                  (ed/split-window-below)
                  (assoc-in [:windows :ratio] 0.3))]
    (session/save-session! state)
    (when-let [sess (session/load-session!)]
      (let [[restored ok] (session/restore-session state sess)]
        (is ok)
        (is (= 7 (get-in restored [:bufs "*test*" :point])))
        (is (= 3 (get-in restored [:bufs "*test*" :mark])))
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

(deftest session-persists-font-lock-states-without-spans
  (let [state (-> (make-state)
                  (assoc-in [:bufs "*test*" :modified] true)
                  (assoc-in [:bufs "*test*" :font-lock]
                            [[[[0 5 :builtin]] :normal]
                             [[[0 2 :string]] :in-string]]))]
    (session/save-session! state)
    (when-let [sess (session/load-session!)]
      (let [[restored ok] (session/restore-session state sess)]
        (is ok)
        (is (= [[nil :normal] [nil :in-string]]
               (get-in restored [:bufs "*test*" :font-lock])))))))

(deftest restore-clamps-offsets-when-the-file-shrank
  ;; Offsets saved for a file that got shorter must not survive restore, or the
  ;; first render sees a point past the end.
  (let [f (java.io.File/createTempFile "xmas-shrink" ".txt")]
    (try
      (spit f "tiny\n")
      (let [path (.getCanonicalPath f)
            sess {:version 1
                  :bufs {path {:name path :text nil :point 2000 :mark 1500
                               :file path :modified false
                               :mode :fundamental :version 7}}
                  :windows {:type :leaf :buffer path :scroll 150 :hscroll 0}
                  :cur-window []
                  :recentf [] :bookmarks {} :mini-history []}
            [restored ok] (session/restore-session (make-state) sess)
            b (get-in restored [:bufs path])]
        (is ok)
        (is (= 5 (:point b)))
        (is (= 5 (:mark b)))
        (is (= 5 (get-in restored [:windows :scroll]))))
      (finally (.delete f)))))

(deftest test-session-save-on-exit
  ;; Simulate clean exit by setting :exit true
  (let [state (-> (make-state)
                  (assoc-in [:bufs "*test*" :point] 8)
                  (assoc :exit true))]
    (when (:exit state)
      (session/save-session! state))
    (when-let [sess (session/load-session!)]
      (let [[restored ok] (session/restore-session state sess)]
        (is ok)
        (is (= 8 (get-in restored [:bufs "*test*" :point])))))))

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

(deftest journal-replays-edits-since-the-snapshot
  (let [dir (temp-dir)]
    (try
      (binding [persist/*dir* dir]
        (let [base (make-state)
              s1   (update-in base [:bufs "*test*"] #(buf/edit % 5 5 ","))
              s2   (update-in s1 [:bufs "*test*"] #(buf/edit % 0 0 ">> "))]
          (session/save-session! base)
          (session/journal-tick! (atom s2))
          (let [sess (session/load-session!)
                [restored ok] (session/restore-session base sess)
                recovered (session/recover-journal! restored)]
            (is ok)
            (is (= ">> hello, world"
                   (str (get-in recovered [:bufs "*test*" :text]))))
            (is (nil? (persist/read-lines "journal"))))))
      (finally (delete-tree dir)))))

(deftest journal-trims-what-the-snapshot-covers
  (let [dir (temp-dir)]
    (try
      (binding [persist/*dir* dir]
        (let [base (make-state)
              s2   (-> base
                       (update-in [:bufs "*test*"] #(buf/edit % 5 5 ","))
                       (update-in [:bufs "*test*"] #(buf/edit % 0 0 ">> ")))
              a    (atom s2)]
          (session/journal-tick! a)
          (is (seq (persist/read-lines "journal")))
          (session/save-session! @a)
          (is (nil? (persist/read-lines "journal")))
          (swap! a update-in [:bufs "*test*"] #(buf/edit % 0 0 "x"))
          (session/journal-tick! a)
          (is (= [{:v 3 :from 0 :to 0 :repl "x" :buf "*test*"}]
                 (persist/read-lines "journal")))))
      (finally (delete-tree dir)))))

(deftest journal-does-not-replay-edits-already-on-disk
  ;; Save after the snapshot, then crash: the file already contains the edit,
  ;; so replaying it again would duplicate it.
  (let [dir (temp-dir)]
    (try
      (binding [persist/*dir* dir]
        (let [f (java.io.File. dir "doc.txt")
              _ (spit f "hello")
              base (update-in (make-state) [:bufs "*test*"]
                              #(assoc % :file (.getPath f) :version 0))
              s1 (update-in base [:bufs "*test*"] #(buf/edit % 5 5 "!"))]
          (session/save-session! base)
          (session/journal-tick! (atom s1))
          (spit f "hello!")
          (session/note-saved! "*test*" 1)
          (let [[restored ok] (session/restore-session base (session/load-session!))
                recovered (session/recover-journal! restored)]
            (is ok)
            (is (= "hello!" (str (get-in recovered [:bufs "*test*" :text]))))
            (is (= 1 (get-in recovered [:bufs "*test*" :version]))))))
      (finally (delete-tree dir)))))

(deftest journal-ignores-corrupt-lines
  (let [dir (temp-dir)]
    (try
      (binding [persist/*dir* dir]
        (let [base (make-state)]
          (session/save-session! base)
          (spit (java.io.File. dir "journal.edn")
                (str "{:broken\n42\n"
                     "{:buf \"*test*\" :v 1 :from 5 :to 5 :repl \"!\"}\n"))
          (let [[restored ok] (session/restore-session base (session/load-session!))
                recovered (session/recover-journal! restored)]
            (is ok)
            (is (= "hello! world"
                   (str (get-in recovered [:bufs "*test*" :text])))))))
      (finally (delete-tree dir)))))

(deftest session-keeps-modified-buffer-with-missing-file
  (let [dir (temp-dir)]
    (try
      (binding [persist/*dir* dir]
        (let [state (-> (make-state)
                        (assoc-in [:bufs "*test*" :file] "/nonexistent/xmas.txt")
                        (assoc-in [:bufs "*test*" :modified] true))]
          (session/save-session! state)
          (let [[restored ok] (session/restore-session (make-state)
                                                       (session/load-session!))]
            (is ok)
            (is (contains? (:bufs restored) "*test*")))))
      (finally (delete-tree dir)))))

(deftest restored-buffers-rederive-language-data
  ;; Language data is derived, not persisted: a restored Python buffer must
  ;; keep highlighting and comment syntax without reopening the file.
  (let [b (#'xmas.session/data->buffer
            {:name "app.py" :text "x = 1\n" :file "/tmp/app.py"
             :mode :python-mode :version 1})]
    (is (= "python" (:id (:language b))))
    (is (= "# " (:comment-start b)))))

(deftest collapsed-folds-survive-restore
  ;; Closed folds are editing state like point, so they belong in the session;
  ;; JSON round-trips a set as a vector, and stale line numbers are dropped.
  (let [snap (#'xmas.session/buffer->data
               (assoc (buf/make "t.clj" "a\nb\nc\nd\n" nil)
                      :mode :clojure-mode :collapsed #{0 3}))
        back (#'xmas.session/data->buffer snap)]
    (is (= #{0 3} (:collapsed back)))
    (let [shrunk (#'xmas.session/data->buffer
                   (assoc snap :text "a\nb\n" :collapsed [0 9]))]
      (is (= #{0} (:collapsed shrunk))))))
