(ns nesmacs.snippet-test
  (:require [clojure.test :refer [deftest is]]
            [nesmacs.snippet :as snippet]))

(deftest plain-text-is-unchanged
  (is (= ["let x = 1" 9] (snippet/expand "let x = 1"))))

(deftest first-placeholder-becomes-the-caret
  (let [[text caret] (snippet/expand "(defn ${1:name} [${2}] ${0})")]
    (is (= "(defn name [] )" text))
    (is (= 6 caret))))

(deftest numbered-placeholder-wins-when-it-comes-first
  (let [[text caret] (snippet/expand "for ${1:i} in ${2:items}:")]
    (is (= "for i in items:" text))
    (is (= 4 caret))))

(deftest dollar-and-brace-placeholders-are-both-supported
  (is (= ["f(a, b)" 2] (snippet/expand "f(${1:a}, ${2:b})"))))

(deftest choice-lists-insert-the-first-item
  (is (= ["emit ok" 7] (snippet/expand "emit ${1|ok,fail|}"))))

(deftest final-dollar-zero-puts-the-caret-at-the-end
  (let [[text caret] (snippet/expand "(do $0)")]
    (is (= "(do )" text))
    (is (= 4 caret))))

(deftest empty-placeholder-is-empty
  (is (= ["x = " 4] (snippet/expand "x = ${1}"))))