(ns xmas.terminal-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [xmas.buf :as buf]
            [xmas.ed :as ed]
            [xmas.persist :as persist]
            [xmas.session :as session]
            [xmas.terminal :as term]
            [xmas.terminal.tmux :as tmux]
            [xmas.window :as win]))

;; Session persistence writes to ~/.xmas by default; keep the real one alone.
(use-fixtures :each
  (fn [f]
    (let [dir (doto (java.io.File/createTempFile "xmas-terminal-test" "")
                (.delete)
                (.mkdirs))]
      (try (binding [persist/*dir* dir] (f))
           (finally (doseq [c (reverse (file-seq dir))] (.delete c)))))))

(defn- make-state
  ([text] (make-state text "fake-1"))
  ([text id]
   {:buf "*term*"
    :bufs {"*term*"
           (assoc (buf/make "*term*" text nil)
                  :mode :terminal-mode :read-only true :journal? false
                  :terminal {:id id :argv ["sh"] :dir "/tmp"
                             :status :starting :create? true
                             :cols 40 :rows 10 :in [] :dirty? true
                             :last-capture 0})}
    :msg nil :mini nil :kill []
    :windows (win/leaf "*term*") :cur-window []
    :rows 12 :cols 40}))

(defn- fake-backend []
  (let [attached? (atom false)
        events (atom [])
        dirty (atom false)
        exited (atom false)
        sent (atom [])
        resized (atom [])
        captures (atom 0)
        killed (atom 0)
        closed (atom 0)
        handle {:id "fake-1"}
        backend (reify term/Backend
                  (attach! [_ spec]
                    (reset! attached? true)
                    (when (:create? spec) handle))
                  (handles [_] (if @attached? {"fake-1" handle} {}))
                  (drain! [_ _]
                    (let [evs @events]
                      (reset! events [])
                      (let [d @dirty]
                        (reset! dirty false)
                        {:events evs :dirty? d :exited? @exited})))
                  (send! [_ _ keys] (swap! sent into keys))
                  (resize! [_ _ c r] (swap! resized conj [c r]))
                  (capture! [_ _] (swap! captures inc))
                  (close! [_ _] (swap! closed inc) (reset! attached? false))
                  (kill! [_ _] (reset! attached? false) (swap! killed inc)))]
    {:backend backend :handle handle :attached? attached? :events events
     :dirty dirty :exited exited :sent sent :resized resized
     :captures captures :killed killed :closed closed}))

;; --- SGR ---

(deftest parses-sgr-into-spans
  (is (= ["abc" [[1 2 {:fg 1 :bg nil :bold false}]]]
         (term/parse-sgr-line "a\u001b[31mb\u001b[0mc")))
  (is (= ["x" [[0 1 {:fg 2 :bg nil :bold true}]]]
         (term/parse-sgr-line "\u001b[1;32mx\u001b[0m")))
  (is (= [[0 1 {:fg 196 :bg nil :bold false}]]
         (second (term/parse-sgr-line "\u001b[38;5;196mx\u001b[0m"))))
  (is (= [[0 1 {:fg [1 2 3] :bg nil :bold false}]]
         (second (term/parse-sgr-line "\u001b[38;2;1;2;3mx\u001b[0m"))))
  (is (= [[0 1 {:fg nil :bg 4 :bold false}]]
         (second (term/parse-sgr-line "\u001b[44mx\u001b[49m"))))
  (is (= [[0 1 {:fg 1 :bg nil :bold false}]]
         (second (term/parse-sgr-line "\u001b[31;%mx")))
      "an unknown SGR parameter is ignored, not fatal"))

(deftest drops-non-sgr-control-sequences
  (is (= ["ab" []] (term/parse-sgr-line "a\u001b[2Kb"))))

;; --- Snapshot -> buffer ---

(deftest snapshot-sets-text-point-title-and-props
  (let [b (-> (buf/make "*term*" "" nil)
              (assoc :terminal {:id "x"} :props []))
        b' (term/apply-snapshot b ["a\u001b[31mb\u001b[0mc" "de"] 1 1 "zsh")]
    (is (= "abc\nde" (str (:text b'))))
    (is (= 5 (:point b')) "line 1 starts at 4, cursor column 1")
    (is (= "zsh" (:title b')))
    (is (= [{:face {:fg 1 :bg nil :bold false} :priority 0 :from 1 :to 2}]
           (:props b')))
    (is (= (inc (or (:version b) 0)) (:version b')))
    (is (= b' (term/apply-snapshot b' ["a\u001b[31mb\u001b[0mc" "de"] 1 1 "zsh"))
        "an identical capture is a no-op")))

(deftest snapshot-clamps-a-cursor-past-the-screen
  (let [b (assoc (buf/make "*t*" "" nil) :terminal {:id "x"})
        b' (term/apply-snapshot b ["ab"] 99 9 nil)]
    (is (= 3 (:point b')))
    (is (nil? (:title b')))))

;; --- Opening and mode input ---

(deftest open-creates-a-terminal-buffer
  (let [s (term/open (make-state "") ["sh"])]
    (is (= "*terminal*" (:buf s)))
    (is (= :terminal-mode (get-in s [:bufs "*terminal*" :mode])))
    (is (true? (get-in s [:bufs "*terminal*" :read-only])))
    (is (str/starts-with? (get-in s [:bufs "*terminal*" :terminal :id]) "xmas-"))
    (is (= :starting (get-in s [:bufs "*terminal*" :terminal :status])))
    (is (= "*terminal-2*" (:buf (term/open s ["sh"]))))))

(deftest terminal-input-is-a-pure-queue
  (let [s (make-state "")]
    (is (= ["a"] (:in (get-in (ed/handle-key s "a") [:bufs "*term*" :terminal]))))
    (is (= [[:ctrl \c]]
           (:in (get-in (-> s (ed/handle-key [:ctrl \c])
                            (ed/handle-key [:ctrl \c]))
                        [:bufs "*term*" :terminal]))))
    (is (= [[:paste "x\ny"]]
           (:in (get-in (ed/handle-key s [:paste "x\ny"])
                        [:bufs "*term*" :terminal]))))
    (is (map? (:pending (ed/handle-key s [:ctrl \c])))
        "C-c is the editor prefix")
    (is (some? (:mini (-> s (ed/handle-key [:ctrl \c])
                           (ed/handle-key [:meta \x]))))
        "C-c M-x opens the minibuffer instead of reaching the shell")
    (is (:exit (-> s (ed/handle-key [:ctrl \c])
                   (ed/handle-key [:ctrl \x])
                   (ed/handle-key [:ctrl \c])))
        "C-c C-x C-c reaches the normal quit command")))

(deftest restart-resets-a-dead-terminal
  (let [s (assoc-in (make-state "") [:bufs "*term*" :terminal :status] :dead)
        s' (term/restart s)]
    (is (= :starting (get-in s' [:bufs "*term*" :terminal :status])))
    (is (true? (get-in s' [:bufs "*term*" :terminal :create?])))))

;; --- tick! ---

(deftest tick-attaches-sends-resizes-and-applies-a-snapshot
  (let [{:keys [backend events sent resized captures]} (fake-backend)
        ed (atom (make-state ""))]
    (term/tick! ed backend)
    (is (= :running (get-in @ed [:bufs "*term*" :terminal :status])))
    (term/tick! ed backend)
    (is (= 1 @captures) "first capture is due because the buffer starts dirty")
    (is (true? (get-in @ed [:bufs "*term*" :terminal :capture-pending?])))
    (reset! events [{:type :snapshot :lines ["$ hi"] :x 2 :y 0 :title "sh"}])
    (term/tick! ed backend)
    (is (= "$ hi" (str (get-in @ed [:bufs "*term*" :text]))))
    (is (= "sh" (get-in @ed [:bufs "*term*" :title])))
    (is (false? (get-in @ed [:bufs "*term*" :terminal :capture-pending?])))
    (swap! ed term/input "echo")
    (term/tick! ed backend)
    (is (= ["echo"] @sent))
    (is (= [] (get-in @ed [:bufs "*term*" :terminal :in])))
    (swap! ed assoc :cols 50)
    (term/tick! ed backend)
    (is (= [[50 10]] @resized))))

(deftest hidden-terminals-are-not-captured
  (let [{:keys [backend captures]} (fake-backend)
        ed (atom (-> (make-state "")
                     (assoc-in [:bufs "*other*"] (buf/make "*other*" "" nil))
                     (assoc :buf "*other*")
                     (assoc :windows (win/leaf "*other*"))))]
    (term/tick! ed backend)
    (term/tick! ed backend)
    (is (zero? @captures) "an invisible terminal gets no capture requests")))

(deftest a-visible-unfocused-terminal-is-captured
  (let [{:keys [backend captures]} (fake-backend)
        [tree _] (win/split (win/leaf "*term*") [] :stacked)
        tree (assoc-in tree [:b :buffer] "*other*")
        ed (atom (-> (make-state "")
                     (assoc-in [:bufs "*other*"] (buf/make "*other*" "" nil))
                     (assoc :buf "*other*" :windows tree :cur-window [:b])))]
    (term/tick! ed backend)
    (term/tick! ed backend)
    (is (= 1 @captures) "an unfocused visible terminal still refreshes")))

(deftest killing-the-buffer-kills-its-session
  (let [{:keys [backend killed attached?]} (fake-backend)
        ed (atom (make-state ""))]
    (term/tick! ed backend)
    (is (true? @attached?))
    (swap! ed (fn [s]
                (-> s
                    (assoc :bufs {"*scratch*" (buf/make "*scratch*" "" nil)})
                    (assoc :buf "*scratch*")
                    (assoc :windows (win/leaf "*scratch*")))))
    (term/tick! ed backend)
    (is (= 1 @killed))
    (is (false? @attached?))))

(deftest a-dead-client-marks-the-buffer-dead
  (let [{:keys [backend exited]} (fake-backend)
        ed (atom (make-state ""))]
    (term/tick! ed backend)
    (reset! exited true)
    (term/tick! ed backend)
    (is (= :dead (get-in @ed [:bufs "*term*" :terminal :status])))))

;; --- Control-mode line parser ---

(defn- fresh-client [pending]
  {:id "t" :pending pending :block nil :out [] :inbox ""
   :attached? true :ready? false :lines nil})

(defn- feed [client lines]
  (reduce (fn [[c events dirty? exited?] line]
            (let [[c events dirty? exited?] (#'tmux/step-line c line events dirty? exited?)]
              [c events dirty? exited?]))
          [client [] false false]
          lines))

(deftest control-mode-skips-the-attach-block
  (let [[c events] (feed (fresh-client [:capture :cursor])
                         ["%begin 1 1 0" "%end 1 1 0"
                          "%begin 1 2 0" "hello" "%end 1 2 0"
                          "%begin 1 3 0" "2\u001f1\u001fsh" "%end 1 3 0"])]
    (is (= [] (:pending c)) "the attach block does not consume a queued token")
    (is (= [{:type :snapshot :lines ["hello"] :x 2 :y 1 :title "sh"}]
           events))))

(deftest control-mode-treats-protocol-looking-lines-as-screen-text
  (let [[c events dirty? exited?]
        (feed (fresh-client [:capture :cursor])
              ["%begin 1 1 0" "%end 1 1 0"
               "%begin 1 2 0" "%exit 1 2 0" "%output %0 hi" "%end 1 2 0"
               "%begin 1 3 0" "0\u001f0" "%end 1 3 0"])]
    (is (false? exited?))
    (is (false? dirty?))
    (is (= [] (:pending c)))
    (is (= {:type :snapshot :lines ["%exit 1 2 0" "%output %0 hi"] :x 0 :y 0 :title nil}
           (first events)))))

(deftest control-mode-error-releases-a-pending-capture
  (let [[c events] (feed (fresh-client [:capture])
                         ["%begin 1 1 0" "%end 1 1 0"
                          "%begin 1 2 0" "%error 1 2 0"])]
    (is (= [] (:pending c)))
    (is (= [{:type :error}] events))))

(deftest control-mode-reports-client-exit
  (is (true? (nth (feed (fresh-client []) ["%exit"]) 3))))

(deftest restart-reattaches-a-live-client
  (let [{:keys [backend attached? closed]} (fake-backend)
        ed (atom (make-state ""))]
    (term/tick! ed backend)
    (term/tick! ed backend)
    (is (true? @attached?))
    (swap! ed term/restart)
    (term/tick! ed backend)
    (is (= 1 @closed) "the old client is closed")
    (is (true? @attached?) "a fresh client attaches in the same tick")
    (is (= :running (get-in @ed [:bufs "*term*" :terminal :status])))))

;; --- tmux key names and session persistence ---

(deftest tmux-key-names
  (is (= "Enter" (tmux/key-name :return)))
  (is (= "C-c" (tmux/key-name [:ctrl \c])))
  (is (= "M-x" (tmux/key-name [:meta \x])))
  (is (= "C-Space" (tmux/key-name [:ctrl \space])))
  (is (nil? (tmux/key-name "text"))))

(deftest session-keeps-the-terminal-identity-not-the-screen
  (let [state (assoc-in (make-state "old screen")
                        [:bufs "*term*" :terminal]
                        {:id "xmas-1" :argv ["sh"] :dir "/tmp"
                         :status :running :cols 80 :rows 24 :in [] :dirty? false})
        _ (session/save-session! state)
        sess (session/load-session!)]
    (is (nil? (get-in sess [:bufs "*term*" :text])))
    (is (= "xmas-1" (get-in sess [:bufs "*term*" :terminal :id])))
    (let [[restored ok] (session/restore-session state sess)]
      (is ok)
      (is (= :terminal-mode (get-in restored [:bufs "*term*" :mode])))
      (is (true? (get-in restored [:bufs "*term*" :read-only])))
      (is (false? (get-in restored [:bufs "*term*" :journal?])))
      (is (= :detached (get-in restored [:bufs "*term*" :terminal :status])))
      (is (= "" (str (get-in restored [:bufs "*term*" :text])))))))