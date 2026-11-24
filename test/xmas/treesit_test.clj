(ns xmas.treesit-test
  (:require [clojure.test :refer [deftest is testing]]
            [xmas.lang :as lang]
            [xmas.treesit :as treesit]))

(defn- spans [id text]
  ;; Include the text in the key: these tests reuse language ids, and the cache
  ;; is per buffer key at a given version.
  (treesit/spans (str "ts-test-" id "-" (hash text)) (lang/language id) text 1))

(deftest python-highlights-cover-comments-strings-and-keywords
  (let [text "def foo(x):\n    # hi\n    return \"s\"\n"
        ss (spans :python text)
        face-at (fn [^long i] (:face (first (filter #(<= (long (:from %)) i (dec (long (:to %)))) ss))))]
    (is (= :keyword (face-at 0)))
    (is (= :comment (face-at (.indexOf text "#"))))
    (is (= :string (face-at (.indexOf text "\"s\""))))))

(deftest clojure-highlights-work
  (let [text "(defn f [x] :kw)\n"
        ss (spans :clojure text)]
    (is (some #(= :keyword (:face %)) ss))
    (is (some #(= :function (:face %)) ss))))

(deftest byte-offsets-become-utf16-offsets
  ;; "café" is 5 bytes but 4 chars; the comment capture must still point at
  ;; the '#' in char space, or colors shift right after non-ASCII text.
  (let [text "# café\n"
        ss (spans :python text)
        c (first (filter #(= :comment (:face %)) ss))]
    (is (= 0 (:from c)))
    (is (= 6 (:to c)))))

(deftest spans-are-sorted-and-within-the-text
  (let [text "(defn f []\n  (let [x 1] x))\n"
        ss (spans :clojure text)
        n (count text)]
    (is (= ss (vec (sort-by (juxt :from :priority) ss))))
    (is (every? (fn [s] (and (<= 0 (long (:from s)))
                             (<= (long (:from s)) (long (:to s)))
                             (<= (long (:to s)) n))) ss))))

(deftest folds-come-from-the-language-query
  (let [text "def a():\n    return 1\n\ndef b():\n    return 2\n"
        fs (treesit/folds "ts-fold-test" (lang/language :python) text 1)]
    (is (seq fs))
    (testing "the fold starts at the def and ends on the body's last line"
      (is (some #(= [0 1] %) fs)))))

(deftest stale-parse-is-reused-until-the-version-changes
  (let [key "ts-version-test"
        l (lang/language :python)
        a (treesit/spans key l "x = 1\n" 7)]
    (testing "same version returns the cached parse, whatever text is passed"
      (is (= a (treesit/spans key l "def completely different(\n" 7))))
    (testing "a version bump re-parses"
      (is (not= a (treesit/spans key l "def completely different(\n" 8))))))

(deftest line-spans-binary-search-includes-overlaps
  (let [ss [{:from 0 :to 3 :face :a} {:from 3 :to 6 :face :b} {:from 10 :to 12 :face :c}]]
    (is (= [0 3] (mapv :from (treesit/line-spans ss 1 5))))
    (is (= [10] (mapv :from (treesit/line-spans ss 10 11))))
    (is (empty? (treesit/line-spans ss 7 9)))
    (is (nil? (treesit/line-spans nil 0 1)))))

(deftest unknown-grammar-falls-back-silently
  (is (nil? (treesit/spans "ts-no-grammar" {:grammar "does.not.Exist" :id "nope"} "x" 1))))
(deftest clojure-lit-folds-nest
  ;; The vendored Clojure folds query is top-level only; folding a form at
  ;; point needs the inner list/vector/map literals too.
  (let [text (str "(defn f [x]\n"
                  "  (let [m {:a 1\n"
                  "           :b 2}]\n"
                  "    (str m)))\n")
        rs (treesit/lit-folds (str "lit-" (hash text)) (lang/language :clojure) text 1)]
    (is (some #{[0 3]} rs) "whole defn")
    (is (some #{[1 2]} rs) "let binding and map")
    (is (not-any? (fn [[s e]] (= s e)) rs))))
