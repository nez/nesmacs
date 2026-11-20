(ns xmas.props-test
  (:require [clojure.test :refer [deftest is]]
            [xmas.props :as props]))

(deftest make-with-defaults
  (let [p (props/make 5 10)]
    (is (= 5 (:from p)))
    (is (= 10 (:to p)))
    (is (= :default (:face p)))))

(deftest adjust-before-edit-unchanged
  (let [ps [(props/make 0 5)]
        ps' (props/adjust ps 10 15 0)]
    (is (= [0 5] [(:from (first ps')) (:to (first ps'))]))))

(deftest adjust-after-edit-shifts-by-delta
  ;; edit at [10 15), replaced with 2 chars → delta = -3
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
    ;; 2 chars replaced with 5 → +3 to the tail
    (is (= [5 18] [(:from (first ps')) (:to (first ps'))]))))

(deftest in-range-filters
  (let [ps [(props/make 0 5) (props/make 10 15) (props/make 20 25)]]
    (is (= [10 15] (let [p (first (props/in-range ps 12 14))]
                     [(:from p) (:to p)])))))

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