(ns xmas.props-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [xmas.props :as props]))

(deftest make-with-defaults
  (let [p (props/make 5 10)]
    (is (= 5 (:from p)))
    (is (= 10 (:to p)))
    (is (= :default (:face p)))
    (is (= 0 (:priority p)))))

(deftest adjust-before-edit-unchanged
  (let [ps [(props/make 0 5)]
        ps' (props/adjust ps 10 15 0)]
    (is (= [0 5] [(:from (first ps')) (:to (first ps'))]))))

(deftest adjust-after-edit-shifts-by-delta
  ;; edit at [10 15), replaced with 2 chars -> delta = -3
  (let [ps [(props/make 20 30)]
        ps' (props/adjust ps 10 15 2)]
    (is (= [17 27] [(:from (first ps')) (:to (first ps'))]))))

(deftest adjust-edit-contains-prop-drops-it
  (let [ps [(props/make 5 8)]
        ps' (props/adjust ps 0 10 0)]
    (is (empty? ps'))))

(deftest adjust-edit-overlaps-start-trims
  (let [ps [(props/make 5 15)]
        ps' (props/adjust ps 0 10 0)]
    (is (= [0 5] [(:from (first ps')) (:to (first ps'))]))))

(deftest adjust-edit-overlaps-end-trims
  (let [ps [(props/make 5 15)]
        ps' (props/adjust ps 10 20 0)]
    (is (= [5 10] [(:from (first ps')) (:to (first ps'))]))))

(deftest adjust-edit-inside-prop-stretches
  (let [ps [(props/make 5 15)]
        ps' (props/adjust ps 8 10 5)]
    ;; 2 chars replaced with 5 -> +3 to the tail
    (is (= [5 18] [(:from (first ps')) (:to (first ps'))]))))

(deftest adjust-insertion-at-prop-start-pushes-right
  (let [ps [(props/make 5 10)]
        ps' (props/adjust ps 5 5 2)]
    (is (= [7 12] [(:from (first ps')) (:to (first ps'))]))))

(deftest adjust-insertion-at-prop-end-does-not-grow
  (let [ps [(props/make 5 10)]
        ps' (props/adjust ps 10 10 2)]
    (is (= [5 10] [(:from (first ps')) (:to (first ps'))]))))

;; --- Exhaustive boundary check over a 7-char model line ---
;;
;; adjust-range is the off-by-one graveyard the roadmap warns about, so cover
;; every prop/edit boundary combination for a small text instead of trusting a
;; handful of examples. `new-len` is the text length after a 7-char line is
;; edited at [e-from e-to) with repl-len characters.
(deftest adjust-every-boundary-combination
  (let [n 7]
    (doseq [from (range n)
            to (range from n)
            e-from (range n)
            e-to (range e-from n)
            repl-len [0 1 3]]
      (let [delta (- repl-len (- e-to e-from))
            new-len (+ n delta)
            out (props/adjust [(props/make from to :x)] e-from e-to repl-len)]
        (is (<= (count out) 1)
            (str "one prop in, at most one out: " [from to e-from e-to repl-len]))
        (when-let [{f :from t :to} (first out)]
          (is (<= 0 f t new-len)
              (str "positions stay inside the new text: "
                   [from to e-from e-to repl-len] " -> " [f t]))
          ;; Unambiguous cases get their exact expected mapping.
          (when (< to e-from)
            (is (= [from to] [f t]) "strictly before the edit is unchanged"))
          (when (> from e-to)
            (is (= [(+ from delta) (+ to delta)] [f t])
                "strictly after the edit shifts by delta"))
          (when (and (< e-from from) (> e-to to))
            (is (= [e-from e-from] [f t])
                "an edit that swallows the prop collapses it")))))))

(defspec adjust-keeps-props-valid 300
  (prop/for-all [len (gen/choose 0 40)]
    (gen/let [a (gen/choose 0 len)
              b (gen/choose 0 len)
              c (gen/choose 0 len)
              d (gen/choose 0 len)
              repl-len (gen/choose 0 10)]
      (let [from (min a b)
            to (max a b)
            e-from (min c d)
            e-to (max c d)
            delta (- repl-len (- e-to e-from))
            new-len (+ len delta)
            out (props/adjust [(props/make from to :x)] e-from e-to repl-len)]
        (and (<= (count out) 1)
             (or (empty? out)
                 (let [{f :from t :to} (first out)]
                   (and (<= 0 f t new-len)))))))))

(defspec adjust-preserves-source-order 200
  (prop/for-all [len (gen/choose 1 60)]
    (gen/let [positions (gen/vector (gen/choose 0 len) 2 12)
              e1 (gen/choose 0 len)
              e2 (gen/choose 0 len)
              repl-len (gen/choose 0 8)]
      (let [ps (props/add-all []
                          (for [p (sort positions)]
                            (props/make p (min len (inc p)) :x)))
            e-from (min e1 e2)
            e-to (max e1 e2)
            out (props/adjust ps e-from e-to repl-len)]
        (= (mapv :from out) (sort (mapv :from out)))))))

(deftest in-range-filters
  (let [ps [(props/make 0 5) (props/make 10 15) (props/make 20 25)]]
    (is (= [10 15] (let [p (first (props/in-range ps 12 14))]
                     [(:from p) (:to p)])))))

(deftest add-all-keeps-position-order
  (let [ps (props/add-all [] [(props/make 10 12 :a)
                              (props/make 0 2 :b)
                              (props/make 5 6 :c)])]
    (is (= [0 5 10] (mapv :from ps)))))

(deftest adjust-preserves-intentionally-empty-props
  (let [ps  [(props/make 5 5 :cursor)]
        ps' (props/adjust ps 20 22 3)]
    (is (= 1 (count ps')))
    (is (= [5 5] [(:from (first ps')) (:to (first ps'))]))))

(deftest remove-owner-leaves-other-props
  (let [ps [(props/make 0 2 :match {:owner :isearch})
            (props/make 4 6 :match {:owner :qr})
            (props/make 8 9 :error)]
        ps' (props/remove-owner ps :isearch)]
    (is (= 2 (count ps')))
    (is (not-any? #(= :isearch (:owner %)) ps'))))