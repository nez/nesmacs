(ns nesmacs.term-test
  (:require [clojure.test :refer [deftest is]]
            [nesmacs.term :as term]))

(defn- byte-source
  "next-byte function over a seq of byte values, -1 when exhausted."
  [bs]
  (let [i (atom 0)
        bs (vec bs)]
    (fn []
      (let [n @i]
        (swap! i inc)
        (if (< n (count bs)) (bs n) -1)))))

(defn- utf8 [^String s] (mapv int (.getBytes s "UTF-8")))

(deftest paste-reader-strips-markers
  (is (= "abc"
         (#'term/read-paste-from
           (byte-source (concat (utf8 "abc") (utf8 "\u001b[201~")))))))

(deftest paste-reader-keeps-partial-terminator-as-text
  ;; ESC[20X is not the end marker; those bytes belong to the payload.
  (is (= "a\u001b[20Xb"
         (#'term/read-paste-from
           (byte-source (concat (utf8 "a\u001b[20Xb") (utf8 "\u001b[201~")))))))

(deftest paste-reader-survives-truncation
  (is (= "ab\u001b[20"
         (#'term/read-paste-from (byte-source (utf8 "ab\u001b[20"))))))

(deftest paste-reader-decodes-utf8
  (is (= "ñ\n"
         (#'term/read-paste-from
           (byte-source (concat (utf8 "ñ\n") (utf8 "\u001b[201~")))))))

(deftest clipboard-seq-is-osc-52
  (is (= "\u001b]52;c;aGk=\u0007" (term/clipboard-seq "hi"))))

(deftest parses-sgr-mouse-click
  (is (= {:type :mouse :button 0 :col 11 :row 4 :action :press}
         (#'term/parse-mouse "<0;12;5" :press)))
  (is (= :release (:action (#'term/parse-mouse "<0;12;5" :release)))))

(deftest wheel-and-motion-are-not-clicks
  (is (= {:type :mouse :action :scroll :direction :up :col 11 :row 4}
         (#'term/parse-mouse "<64;12;5" :press)))
  (is (= :down (:direction (#'term/parse-mouse "<69;12;5" :press))))
  (is (nil? (#'term/parse-mouse "<64;12;5" :release)))
  (is (nil? (#'term/parse-mouse "<66;12;5" :press)))
  (is (nil? (#'term/parse-mouse "<32;12;5" :press)))
  (is (nil? (#'term/parse-mouse "<0;12" :press))))

(deftest set-clipboard-skips-oversized
  (let [out (atom "")]
    (binding [term/*out-fn* (fn [s] (swap! out str s))]
      (term/set-clipboard! (apply str (repeat 100001 "x")))
      (is (= "" @out))
      (term/set-clipboard! "hi")
      (is (= (term/clipboard-seq "hi") @out)))))
(deftest sg-supports-palette-and-truecolor
  (let [out (atom "")]
    (binding [term/*out-fn* #(swap! out str %)]
      (term/sg {:fg 1})
      (is (= "\u001b[0;38;5;1m" @out))
      (reset! out "")
      (term/sg {:fg [1 2 3] :bold true})
      (is (= "\u001b[0;1;38;2;1;2;3m" @out)))))

(deftest resize-is-applied-once
  (term/note-resize!)
  (is (= [24 80] (term/apply-pending-resize! (fn [] [24 80]))))
  (is (nil? (term/apply-pending-resize! (fn [] [1 1])))))
