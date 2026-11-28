(ns nesmacs.text
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.util Arrays]))

(defn next-pos
  "Advance past one codepoint."
  ^long [^CharSequence t ^long p]
  (if (>= p (.length t)) p
    (+ p (Character/charCount (Character/codePointAt t (int p))))))

(defn prev-pos
  "Retreat past one codepoint."
  ^long [^CharSequence t ^long p]
  (if (<= p 0) 0
    (- p (Character/charCount (Character/codePointBefore t (int p))))))

(defonce ^:private cell-widths
  (let [{:keys [zero wide]} (edn/read-string (slurp (io/resource "nesmacs/unicode11-widths.edn")))
        ^bytes widths (byte-array 0x110000)]
    (Arrays/fill widths (byte 1))
    (doseq [[ranges width] [[zero 0] [wide 2]]
            [start end] ranges]
      (Arrays/fill widths (int start) (inc (int end)) (byte width)))
    widths))

(defn char-width
  "Terminal columns for a codepoint, using the same widths as the web renderer.
   Raw control bytes occupy one cell because the painter replaces them."
  ^long [^long cp]
  (if (<= 0 cp 0x10FFFF) (aget ^bytes cell-widths (int cp)) 1))

(defn display-width
  "Total terminal columns for text[from..to). The 1-arg overload measures
   the whole CharSequence — a common enough pattern that spelling it out
   as (display-width t 0 (count t)) at each call site was noise."
  (^long [^CharSequence t] (display-width t 0 (.length t)))
  (^long [^CharSequence t ^long from ^long to]
   (loop [p from w 0]
     (if (>= p to) w
       (let [cp (Character/codePointAt t (int p))]
         (recur (+ p (Character/charCount cp)) (+ w (char-width cp))))))))

(defn word-forward
  "Position after the next word boundary."
  ^long [^CharSequence t ^long p]
  (loop [p p in false]
    (if (>= p (.length t)) (.length t)
      (let [cp (Character/codePointAt t (int p))
            wc (Character/isLetterOrDigit cp)]
        (if (and in (not wc)) p
          (recur (next-pos t p) wc))))))

(defn word-backward
  "Position before the previous word boundary."
  ^long [^CharSequence t ^long p]
  (loop [p p in false]
    (if (<= p 0) 0
      (let [cp (Character/codePointBefore t (int p))
            wc (Character/isLetterOrDigit cp)]
        (if (and in (not wc)) p
          (recur (prev-pos t p) wc))))))

(defn pos-at-col
  "Position in t where display column reaches col, scanning from..to."
  ^long [^CharSequence t ^long from ^long to ^long col]
  (loop [p from w 0]
    (if (or (>= p to) (>= w col)) p
      (let [cp (Character/codePointAt t (int p))
            cw (char-width cp)
            nw (+ w cw)]
        (if (> nw col) p
          (recur (+ p (Character/charCount cp)) nw))))))

(defn- match-at?
  "Does pattern match t at position i?"
  [^CharSequence t ^String pattern ^long i]
  (let [pn (.length pattern)]
    (loop [j (int 0)]
      (cond
        (>= j pn) true
        (= (.charAt t (int (+ i j))) (.charAt pattern j)) (recur (unchecked-inc-int j))
        :else false))))

(defn search-forward
  "Position of pattern in t at or after from, or nil."
  [^CharSequence t ^String pattern ^long from]
  (when (seq pattern)
    (let [tn (.length t)
          pn (.length pattern)]
      (loop [i from]
        (when (<= (+ i pn) tn)
          (if (match-at? t pattern i) i (recur (inc i))))))))

(defn search-backward
  "Position of the latest `pattern` match whose END is <= `from`, or nil.
   A match straddling `from` (ending past it) is not returned."
  [^CharSequence t ^String pattern ^long from]
  (when (and (seq pattern) (pos? from))
    (let [pn (.length pattern)
          tn (.length t)]
      (when (<= pn tn)
        (loop [i (long (- (min from tn) pn))]
          (when (>= i 0)
            (if (match-at? t pattern i) i (recur (dec i)))))))))
