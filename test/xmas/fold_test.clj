(ns xmas.fold-test
  (:require [clojure.test :refer [deftest is testing]]
            [xmas.fold :as fold]
            [xmas.gap :as gap]))

(defn- ranges-for [text mode]
  (fold/ranges "fold-test" {:mode mode} (gap/of text) 1))

(deftest lisp-fallback-folds-multiline-top-level-forms
  (let [rs (ranges-for "(defn a []\n  1)\n\n(def b\n  2)\n" :clojure-mode)]
    (is (= [[0 1] [3 4]] rs))))

(deftest lisp-fallback-ignores-single-line-forms
  (is (= [] (ranges-for "(a) (b)\n(c)\n" :clojure-mode))))

(deftest lisp-ranges-handle-strings-with-parens
  ;; The fallback is a paren-depth scanner, not a parser: a `)` inside a
  ;; string closes the form early. Tree-sitter covers every real language.
  (is (= [[0 1]] (ranges-for "(str \"a\n)\" b\n)" :clojure-mode))))

(deftest no-folds-for-plain-text
  (is (= [] (ranges-for "(a\nb)" :fundamental-mode))))

(deftest hidden-and-visible-math
  (let [folds [[2 5] [8 9]]]
    (testing "the start line of a fold stays visible; the end line is hidden"
      (is (not (fold/hidden? folds 2)))
      (is (fold/hidden? folds 3))
      (is (fold/hidden? folds 5))
      (is (fold/hidden? folds 9))
      (is (not (fold/hidden? folds 6))))
    (is (= 3 (fold/visible-before folds 4)))
    (is (= 3 (fold/visible-before folds 5)))
    (is (= 8 (fold/visible-count folds 12)))
    (is (= 7 (fold/line-at-display folds 4)))
    (is (= 2 (fold/visible-start folds 4)))
    (is (= [[8 9]] (vec (fold/effective folds 3))))))

(deftest toggle-picks-the-smallest-covering-range
  (let [ranges [[0 10] [2 4] [6 7]]]
    (is (= #{2} (fold/toggle ranges #{} 3)))
    (is (= #{} (fold/toggle ranges #{2} 3)))
    (testing "a line outside every range changes nothing"
      (is (= #{7} (fold/toggle ranges #{7} 20))))))