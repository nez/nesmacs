(ns nesmacs.rpc-test
  (:require [clojure.test :refer [deftest is testing]]
            [nesmacs.proc :as proc]
            [nesmacs.rpc :as rpc]))

(deftest frame-round-trips
  (let [msg {:seq 1 :type "request" :command "initialize"}
        [msgs rest*] (rpc/decode-msgs (rpc/encode-msg msg))]
    (is (= [msg] msgs))
    (is (= "" rest*))))

(deftest content-length-counts-utf8-bytes
  (testing "a raw multibyte frame is sliced by byte count (as adapters send it)"
    (let [body "{\"seq\":1,\"type\":\"event\",\"body\":{\"output\":\"café 日本\"}}"
          len (alength (.getBytes body "UTF-8"))
          framed (str "Content-Length: " len "\r\n\r\n" body)
          [msgs rest*] (rpc/decode-msgs framed)]
      (is (> len (count body)))
      (is (= "café 日本" (get-in (first msgs) [:body :output])))
      (is (= "" rest*)))))

(deftest a-partial-frame-waits-for-more-bytes
  (let [framed (rpc/encode-msg {:seq 2 :type "response"})
        cut (quot (count framed) 2)
        [msgs rest*] (rpc/decode-msgs (subs framed 0 cut))]
    (is (= [] msgs))
    (is (= (subs framed 0 cut) rest*))
    (let [[msgs2 rest*2] (rpc/decode-msgs (str rest* (subs framed cut)))]
      (is (= [{:seq 2 :type "response"}] msgs2))
      (is (= "" rest*2)))))

(deftest two-frames-in-one-read
  (let [[msgs rest*] (rpc/decode-msgs (str (rpc/encode-msg {:seq 1})
                                           (rpc/encode-msg {:seq 2})))]
    (is (= [{:seq 1} {:seq 2}] msgs))
    (is (= "" rest*))))

(deftest malformed-json-is-dropped-not-fatal
  (let [framed (str "Content-Length: 4\r\n\r\nnope"
                    (rpc/encode-msg {:seq 3}))
        [msgs rest*] (rpc/decode-msgs framed)]
    (is (= [{:seq 3}] msgs))
    (is (= "" rest*))))
(deftest every-byte-can-arrive-in-a-separate-read
  (let [body "{\"output\":\"añ€𝄞\"}"
        frame (str "Content-Length: " (alength (.getBytes body "UTF-8")) "\r\n\r\n" body)
        conn {:kind :stdio :inbox (atom (byte-array 0))}
        msgs (mapcat (fn [byte]
                       (with-redefs [proc/drain-bytes! (fn [_ _] (byte-array [byte]))]
                         (rpc/read! conn)))
                     (.getBytes frame "UTF-8"))]
    (is (= [{:output "añ€𝄞"}] msgs))
    (is (zero? (alength ^bytes @(:inbox conn))))))
