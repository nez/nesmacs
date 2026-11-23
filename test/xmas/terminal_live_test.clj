(ns ^:integration xmas.terminal-live-test
  "End-to-end check against the real tmux binary. Excluded from `make test`
   because tmux may not be installed; run `make term`."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [xmas.buf :as buf]
            [xmas.terminal :as term]
            [xmas.terminal.tmux :as tmux]
            [xmas.window :as win]))

(defn- tmux-on-path? []
  (some (fn [dir]
          (let [f (java.io.File. ^String (str dir java.io.File/separator "tmux"))]
            (and (.isFile f) (.canExecute f))))
        (str/split (or (System/getenv "PATH") "")
                   (re-pattern java.io.File/pathSeparator))))

(defn- tmux! [socket & args]
  (let [p (.start (ProcessBuilder.
                   (into-array String (into ["tmux" "-L" socket] args))))]
    (.close (.getOutputStream p))
    (.waitFor p)))

(defn- session-exists? [socket id]
  (zero? (tmux! socket "has-session" "-t" id)))

(defn- base-state []
  {:buf "*scratch*"
   :bufs {"*scratch*" (buf/make "*scratch*" "" nil)}
   :msg nil :mini nil :kill []
   :windows (win/leaf "*scratch*") :cur-window []
   :rows 24 :cols 80})

(defn- text [s] (str (get-in s [:bufs "*terminal*" :text])))

(defn- pump!
  "Tick the editor loop until `pred` accepts the state or the deadline passes."
  [editor pred ms]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop []
      (term/tick! editor tmux/backend)
      (cond (pred @editor) true
            (> (System/currentTimeMillis) end) false
            :else (do (Thread/sleep 50) (recur))))))

(defn- with-private-server [f]
  (let [socket (str "xmas-live-" (System/nanoTime))
        conf   (str (System/getProperty "java.io.tmpdir")
                    "/xmas-live-" (System/nanoTime) ".conf")]
    (binding [tmux/*socket* socket
              tmux/*config* conf]
      (try (f socket)
           (finally
             (tmux/kill-all-sessions!)
             (tmux! socket "kill-server")
             (.delete (java.io.File. conf)))))))

(deftest ^:integration live-terminal-round-trip
  (if-not (tmux-on-path?)
    (is false "tmux is not on PATH; `make term` needs it")
    (with-private-server
      (fn [socket]
        (let [editor (atom (base-state))
              id     (do (swap! editor term/open ["sh"])
                         (get-in @editor [:bufs "*terminal*" :terminal :id]))]
          (is (pump! editor #(re-find #"[$#%]" (text %)) 15000)
              "the shell prompt is captured")
          (is (session-exists? socket id))

          (doseq [k ["echo xmas-live" :return]]
            (swap! editor term/input k))
          (is (pump! editor #(str/includes? (text %) "xmas-live") 15000)
              "typed keys reach the shell and their output comes back")

          ;; Closing the client is what editor exit does; the session stays.
          (tmux/close! tmux/backend (get (tmux/handles tmux/backend) id))
          (is (session-exists? socket id) "closing the client keeps the session")

          ;; Restoring a saved session must attach only, never create: if the tmux
          ;; session is gone the buffer is marked dead instead of showing a
          ;; fresh shell that would look like restored state.
          (swap! editor update-in [:bufs "*terminal*" :terminal]
                 #(assoc % :status :detached :create? false))
          (is (pump! editor (fn [s]
                              (and (some? (get (tmux/handles tmux/backend) id))
                                   (= :running
                                      (get-in s [:bufs "*terminal*" :terminal :status]))))
                     15000)
              "restore attaches to the surviving session")
          (doseq [k ["echo reattached" :return]]
            (swap! editor term/input k))
          (is (pump! editor #(str/includes? (text %) "reattached") 15000)
              "the shell survived closing the client")

          ;; Killing the buffer reaps the session on the next tick.
          (swap! editor update :bufs dissoc "*terminal*")
          (is (pump! editor (fn [_] (not (session-exists? socket id))) 10000)
              "killing the buffer destroys its session"))))))