(ns nesmacs.web-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [org.httpkit.server :as http]
            [nesmacs.buf :as buf]
            [nesmacs.term :as term]
            [nesmacs.web :as web]
            [nesmacs.window :as win]))

(defn make-state [text]
  {:buf "*test*"
   :bufs {"*test*" (assoc (buf/make "*test*" text nil) :point 0)}
   :kill [] :msg nil :mini nil
   :windows (win/leaf "*test*") :cur-window []
   :rows 24 :cols 80})

(deftest render-to-string-produces-output
  (let [s (make-state "hello\nworld")
        result (web/render-to-string s)]
    (is (string? result))
    (is (pos? (count result)))
    (is (.contains result "hello"))))

(deftest render-to-string-contains-mode-line
  (let [s (make-state "test")
        result (web/render-to-string s)]
    (is (.contains result "*test*"))))

(deftest render-to-string-does-not-corrupt-term-out
  (let [s (make-state "abc")
        ;; capture that real term/out is unaffected
        before (str term/real-out)]
    (web/render-to-string s)
    (is (= before (str term/real-out)))))

(deftest render-sanitizes-buffer-control-bytes
  (let [payload (str (char 27) "[31mPWN")
        result (web/render-to-string (make-state payload))]
    (is (not (.contains result payload)))
    (is (.contains result "�[31mPWN"))))

(defn- event [& fields]
  (#'web/parse-event (json/write-str fields)))

(deftest websocket-keys-preserve-editor-key-types
  (is (= [:ctrl \x] (event "key" "ctrl" "x")))
  (is (= [:meta \:] (event "key" "meta" ":")))
  (is (= :page-down (event "key" "special" "PageDown")))
  (is (= \2 (event "text" "2")))
  (is (= \ñ (event "text" "ñ")))
  (is (= "😀" (event "text" "😀")))
  (is (= [:paste "日本語"] (event "text" "日本語")))
  (is (= [:paste "a\nb"] (event "paste" "a\nb")))
  (is (nil? (event "key" "ctrl" "xx")))
  (is (nil? (event "paste" 123)))
  (is (nil? (#'web/parse-event "{}"))))

(deftest websocket-viewport-events-are-validated
  (is (= {:type :mouse :action :scroll :direction :down :row 7 :col 120}
         (event "mouse" "down" 7 120)))
  (is (= {:type :mouse :action :press :button 0 :row 7 :col 120}
         (event "mouse" "click" 7 120)))
  (is (nil? (event "mouse" "up" -1 3)))
  (is (nil? (event "mouse" "left" 1 3)))
  (is (= {:type :resize :rows 42 :cols 132} (event "resize" 42 132)))
  (is (nil? (event "resize" 50000 132))))

(deftest server-binds-to-loopback
  (let [opts (atom nil)
        editor (atom (make-state ""))]
    (with-redefs [http/run-server (fn [_handler options]
                                    (reset! opts options)
                                    (fn []))]
      (let [srv (web/start! editor 1234 identity)]
        (is (= "127.0.0.1" (:ip @opts)))
        (web/stop! srv)))))

(deftest serves-packaged-resources
  (doseq [[_ [resource _]] @#'web/assets]
    (is (some? (io/resource (str "nesmacs/" resource))))))

(deftest websocket-requires-the-local-editor-origin
  (with-redefs [http/as-channel (fn [_ _] :upgraded)]
    (let [request {:uri "/ws" :request-method :get
                   :headers {"host" "localhost:1234" "origin" "http://localhost:1234"}}
          dispatch (partial #'web/handler nil nil)]
      (is (= :upgraded (dispatch request)))
      (is (= 403 (:status (dispatch (assoc-in request [:headers "origin"] "http://localhost:9999")))))
      (is (= 403 (:status (dispatch (assoc request :headers {"host" "editor.example"}))))))))
