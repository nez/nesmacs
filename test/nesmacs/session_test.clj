(ns nesmacs.session-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [nesmacs.ed :as ed]
            [nesmacs.session :as session]
            [nesmacs.buf :as buf]
            [nesmacs.persist :as persist]
            [nesmacs.testutil :as tu]
            [nesmacs.window :as win]
            [nesmacs.cmd :as cmd]
            [nesmacs.complete :as complete]
            [nesmacs.dap :as dap]))

(defn- temp-dir [] (tu/temp-dir "nesmacs-session-dir"))
(defn- delete-tree [f] (tu/delete-tree f))

;; Never read or write the real ~/.nesmacs from tests; the journal tests in
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

(deftest a-restored-session-selects-the-buffer-it-shows
  ;; Regression: `:buf` stayed at the pre-restore buffer while the window tree
  ;; pointed at another one, so keys went to a buffer that was not on screen.
  (let [state (-> (make-state)
                  (assoc-in [:bufs "term"] (buf/make "term" "" nil))
                  (assoc :windows (win/leaf "term") :cur-window []))]
    (session/save-session! state)
    (when-let [sess (session/load-session!)]
      (let [[restored ok] (session/restore-session (make-state) sess)]
        (is ok)
        (is (= "term" (:buf restored)))))))

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
  (let [f (java.io.File/createTempFile "nesmacs-shrink" ".txt")]
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
  (let [f (java.io.File/createTempFile "nesmacs-session" ".txt")]
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
  (let [f (java.io.File/createTempFile "nesmacs-session" ".txt")]
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
          (is (= [{:v 3 :from 0 :to 0 :repl "x" :buf "*test*"
                   :journal-id (get-in @a [:bufs "*test*" :journal-id])}]
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
          (session/note-saved! "*test*" (get-in s1 [:bufs "*test*"]))
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
        (let [base (update-in (make-state) [:bufs "*test*"] dissoc :journal-id)]
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
                        (assoc-in [:bufs "*test*" :file] "/nonexistent/nesmacs.txt")
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
  (let [b (#'nesmacs.session/data->buffer
            {:name "app.py" :text "x = 1\n" :file "/tmp/app.py"
             :mode :python-mode :version 1})]
    (is (= "python" (:id (:language b))))
    (is (= "# " (:comment-start b)))))

(deftest collapsed-folds-survive-restore
  ;; Closed folds are editing state like point, so they belong in the session;
  ;; JSON round-trips a set as a vector, and stale line numbers are dropped.
  (let [snap (#'nesmacs.session/buffer->data
               (assoc (buf/make "t.clj" "a\nb\nc\nd\n" nil)
                      :mode :clojure-mode :collapsed #{0 3}))
        back (#'nesmacs.session/data->buffer snap)]
    (is (= #{0 3} (:collapsed back)))
    (let [shrunk (#'nesmacs.session/data->buffer
                   (assoc snap :text "a\nb\n" :collapsed [0 9]))]
      (is (= #{0} (:collapsed shrunk))))))

(deftest theme-and-breakpoints-survive-a-session-round-trip
  (let [state (assoc (make-state) :theme :light)]
    (dap/set-breakpoints! "/tmp/nesmacs-session-bp.py" [4 1])
    (try
      (session/save-session! state)
      (let [sess (session/load-session!)]
        (is (= :light (:theme sess)))
        (is (= {"/tmp/nesmacs-session-bp.py" #{1 4}} (:breakpoints sess)))
        (let [[restored ok] (session/restore-session state sess)]
          (is ok)
          (is (= :light (:theme restored)))))
      (finally (dap/restore-breakpoints! {})))))

(deftest a-corrupt-session-does-not-brick-startup
  ;; `win/leaves` recursed forever on nil, so a session without a window tree
  ;; (or with garbage field types) killed the editor before the first frame.
  (let [[s ok] (session/restore-session
                 (make-state)
                 (#'session/sanitize-session
                   {:version 1
                  :bufs {"*scratch*" {:name "*scratch*" :text "hi" :point "4"
                                      :version "x" :file 42}
                         "junk" "not a buffer"}
                    :windows nil
                    :cur-window [:nope]}))]
    (is ok)
    (is (= "*scratch*" (:buf s)))
    (is (= 0 (get-in s [:bufs "*scratch*" :point])))
    (is (= [[]] (win/leaves (:windows s))))
    (is (= ["*scratch*"] (map #(get-in (:windows s) (conj % :buffer))
                              (win/leaves (:windows s)))))))

(deftest all-buffers-vanished-falls-back-to-a-scratch-buffer
  ;; A window is rebuilt as *scratch* when its file is gone. If no buffer
  ;; survives and *scratch* was not in the session, the renderer used to get
  ;; nil for the leaf's buffer and crash on the first frame.
  (let [[s ok] (session/restore-session
                 (make-state)
                 (#'session/sanitize-session
                   {:version 1
                    :bufs {"gone.clj" {:name "gone.clj" :file "/no/such/gone.clj"
                                       :modified false}}
                    :windows {:type :leaf :buffer "gone.clj"}
                    :cur-window []}))]
    (is ok)
    (is (= "*scratch*" (:buf s)))
    (is (contains? (:bufs s) "*scratch*"))))

(deftest a-torn-journal-record-is-skipped
  ;; from/to are fed to buf/edit; a string there used to abort restore.
  (let [s (make-state)
        b (get-in s [:bufs "*test*"])
        s' (#'session/apply-record s {:buf "*test*" :journal-id (:journal-id b)
                                     :v 1 :from "x" :to 2 :repl "z"})]
    (is (= (str (:text b)) (str (get-in s' [:bufs "*test*" :text]))))
    (is (= (:version b) (get-in s' [:bufs "*test*" :version])))))

(deftest reopening-a-buffer-does-not-recover-edits-from-its-old-instance
  (let [file (java.io.File. persist/*dir* "reopened.txt")
        path (.getPath file)]
    (spit file "original")
    (let [first-open (ed/find-file (make-state) path)
          reopened (ed/find-file (make-state) path)]
      (session/journal-tick! (atom (ed/edit first-open 0 0 "discarded ")))
      (session/save-session! reopened)
      (session/journal-tick! (atom (ed/edit reopened 0 0 "kept ")))
      (let [[restored] (session/restore-session (make-state) (session/load-session!))]
        (is (= "kept original" (str (:text (cmd/cur (session/recover-journal! restored))))))))))

(deftest restored-directory-has-live-entries-and-protection
  (let [dir (.getPath persist/*dir*)
        s (ed/find-file (make-state) dir)]
    (session/save-session! s)
    (spit (java.io.File. dir "added.txt") "added")
    (let [[restored] (session/restore-session (make-state) (session/load-session!))
          b (cmd/cur restored)]
      (is (:read-only b))
      (is (false? (:journal? b)))
      (is (some #(= "added.txt" (.getName ^java.io.File %)) (:listbuf-entries b))))))

(deftest a-session-keeps-cached-files-and-their-editing-flags
  (let [file (java.io.File. persist/*dir* "removed.txt")
        path (.getPath file)]
    (spit file "cached")
    (let [s (-> (ed/find-file (make-state) path)
                (assoc-in [:bufs path :read-only] true)
                (ed/mini-start "Command: " (fn [s _] s))
                (ed/handle-key \x))]
      (is (.delete file))
      (session/save-session! s)
      (let [snapshot (session/load-session!)
            [restored] (session/restore-session (make-state) snapshot)]
        (is (not (contains? (:bufs snapshot) " *mini*")))
        (is (= path (:buf restored)))
        (is (= "cached" (str (:text (cmd/cur restored)))))
        (is (:read-only (cmd/cur restored)))))))

(deftest a-vanished-buffer-does-not-fall-back-to-a-missing-scratch
  ;; *scratch* may have been killed, so the fallback must be a buffer that
  ;; actually exists; otherwise the renderer dereferences nil.
  (let [[s ok] (session/restore-session
                 (make-state)
                 (#'session/sanitize-session
                   {:version 1
                    :bufs {"kept.clj" {:name "kept.clj" :text "x"
                                       :file "/no/such/kept.clj"
                                       :modified true}}
                    :windows {:type :leaf :buffer "gone.clj"}
                    :cur-window []}))]
    (is ok)
    (is (= "kept.clj" (:buf s)))
    (is (= "kept.clj" (get-in s [:windows :buffer])))
    (is (contains? (:bufs s) "kept.clj"))))

(deftest failed-session-write-does-not-mark-snapshot-saved
  (let [editor (atom (make-state))
        saves (atom [])
        succeed? (atom false)]
    (with-redefs-fn {#'session/last-snapshot (atom nil)
                     #'session/save-counter (atom 0)
                     #'session/save-interval 2
                     #'session/save-pending? (atom false)
                     #'clojure.core/future-call (fn [f] (f))
                     #'persist/save-quiet! (fn [_ snapshot]
                                             (swap! saves conj (:theme snapshot))
                                             @succeed?)}
      (fn []
        (session/save-tick! editor)
        (reset! succeed? true)
        (session/save-tick! editor)
        (session/save-tick! editor)
        (is (= [:default :default] @saves))
        (swap! editor assoc :theme :light)
        (session/save-tick! editor)
        (is (= [:default :default :light] @saves))))))

(deftest restores-crlf-for-clean-and-dirty-buffers
  (let [file (java.io.File. persist/*dir* "crlf.txt")
        path (.getCanonicalPath file)]
    (spit file "one\r\ntwo\r\n")
    (let [state (ed/find-file (make-state) path)]
      (doseq [s [state (ed/edit state 0 3 "edited")]]
        (is (session/save-session! s))
        (let [[restored ok] (session/restore-session (make-state) (session/load-session!))
              b (get-in restored [:bufs path])]
          (is ok)
          (is (= :crlf (:line-ending b)))
          (is (= (str (get-in s [:bufs path :text])) (str (:text b)))))))))
