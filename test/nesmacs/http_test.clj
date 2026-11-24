(ns nesmacs.http-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [org.httpkit.server :as server]
            [nesmacs.buf :as buf]
            [nesmacs.ed :as ed]
            [nesmacs.http :as http]))

(deftest a-request-is-the-block-above-the-first-blank-line
  (is (= {:method "POST"
          :url "https://example.com/a?b=c"
          :headers {"Accept" "text/plain" "X-Token" "abc"}}
         (http/parse-request
           "POST https://example.com/a?b=c\nAccept: text/plain\nX-Token:   abc  \n\nbody is ignored\n"))))

(deftest only-http-urls-are-accepted
  (testing "no request line at all"
    (is (nil? (http/parse-request "")))
    (is (nil? (http/parse-request "\n\n"))))
  (testing "other schemes and relative paths would need a second transport"
    (is (nil? (http/parse-request "GET file:///etc/passwd\n")))
    (is (nil? (http/parse-request "GET /local/path\n")))))

(deftest a-response-renders-status-sorted-headers-and-body
  (is (= "HTTP 200\na: 1\nb: 2\n\nhello\n"
         (http/render {:status 200
                       :headers {:b "2" :a "1"}
                       :body "hello"})))
  (testing "a transport error still says what failed"
    (is (str/starts-with? (http/render {:url "http://x/" :error (Exception. "boom")})
                          "GET http://x/ failed: boom"))))

(deftest a-huge-body-is-capped
  (let [out (http/render {:status 200 :headers {} :body (apply str (repeat 300000 "x"))})]
    (is (str/includes? out "[truncated: 300000 bytes]"))
    (is (< (count out) 210000))))

(deftest send-fills-the-response-area-below-the-request
  ;; The real path end to end: future -> http-kit -> swap! into the editor atom.
  ;; The response area is everything after the request's blank line, so a
  ;; second C-c C-c parses the request again, not the previous response.
  (let [srv (server/run-server
              (fn [_req]
                {:status 200
                 :headers {"Content-Type" "text/plain"}
                 :body "hello"})
              {:ip "127.0.0.1" :port 0})]
    (try
      (let [port (:local-port (meta srv))
            text (str "GET http://127.0.0.1:" port "/x\nAccept: */*\n\n")
            state {:bufs {"*HTTP*" (buf/make "*HTTP*" text nil)}}
            ed (atom state)
            [sent message] (http/send! state ed)]
        ;; The command's return value is what the editor stores; the reply only
        ;; applies while that state still carries this request's id.
        (reset! ed sent)
        (is (= (str "GET http://127.0.0.1:" port "/x …") message))
        (loop [n 0]
          (let [t (str (get-in @ed [:bufs "*HTTP*" :text]))]
            (when (and (not (str/includes? t "HTTP 200")) (< n 100))
              (Thread/sleep 50)
              (recur (inc n)))))
        (let [t (str (get-in @ed [:bufs "*HTTP*" :text]))]
          (is (str/starts-with? t (str "GET http://127.0.0.1:" port "/x\nAccept: */*\n\n"))
              "the request stays intact above the response")
          (is (str/includes? t "HTTP 200"))
          (is (str/includes? t "hello")))
        (is (= "GET" (:method (http/parse-request
                                (str (get-in @ed [:bufs "*HTTP*" :text])))))
            "a second send re-parses the request, not the response"))
      (finally
        (srv)))))
(deftest the-c-c-c-c-wraps-the-result-as-a-message
  ;; The keymap runs inside `swap!`, so the command must return state, not
  ;; print: an invalid buffer only produces a minibuffer message and no future.
  (let [state {:bufs {"*HTTP*" (buf/make "*HTTP*" "not a request\n\n" nil)}}]
    (is (str/starts-with? (:msg (ed/http-send state)) "No http(s) request line"))))

(deftest an-edited-request-survives-the-reply
  ;; A reply must only own the response area; the request is the user's text.
  (let [b (-> (buf/make "*HTTP*" "GET http://x/\nX: 1\n\nold\n" nil)
              (assoc :http-request 7 :point 5))
        s (#'http/with-response {:bufs {"*HTTP*" b}} "*HTTP*"
                                "HTTP 200\n\nok\n" 7)
        t (str (get-in s [:bufs "*HTTP*" :text]))]
    (is (str/starts-with? t "GET http://x/\nX: 1\n\n"))
    (is (str/ends-with? t "HTTP 200\n\nok\n"))
    (is (not (str/includes? t "old")))
    (is (= 5 (get-in s [:bufs "*HTTP*" :point])))))

(deftest a-stale-reply-cannot-overwrite-a-newer-one
  (let [b (-> (buf/make "*HTTP*" "GET http://x/\n\n" nil)
              (assoc :http-request 8))
        s {:bufs {"*HTTP*" b}}]
    (is (identical? s (#'http/with-response s "*HTTP*" "HTTP 500\n\n" 7)))))

(deftest a-request-without-a-blank-line-still-gets-one
  (let [b (buf/make "*HTTP*" "GET http://x/" nil)
        s (#'http/with-response {:bufs {"*HTTP*" b}} "*HTTP*" "HTTP 200\n\nok\n" nil)
        t (str (get-in s [:bufs "*HTTP*" :text]))]
    (is (= "GET http://x/\n\nHTTP 200\n\nok\n" t))))

(deftest only-a-real-method-is-accepted
  (is (nil? (http/parse-request "F@O http://x/\n")))
  (is (nil? (http/parse-request "http://x/\n")))
  (is (= "GET" (:method (http/parse-request "GET http://x/\n")))))
