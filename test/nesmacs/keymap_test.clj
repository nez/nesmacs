(ns nesmacs.keymap-test
  (:require [clojure.test :refer [deftest is]]
            [nesmacs.keymap :as keymap]))

(deftest escaped-syntax-is-rejected
  (is (thrown? clojure.lang.ExceptionInfo (keymap/parse-key-string "\\C-c")))
  (is (thrown? clojure.lang.ExceptionInfo (keymap/parse-key-string "\\C-x\\C-s"))))

(deftest parses-standard-syntax
  (is (= [[:ctrl \x] [:ctrl \f]] (keymap/parse-key-string "C-x C-f")))
  (is (= [[:ctrl \space] :return :up]
         (keymap/parse-key-string "C-SPC RET <up>")))
  (is (= [\a \b \c] (keymap/parse-key-string "abc")))
  (is (= [] (keymap/parse-key-string "   "))))

(deftest unknown-key-signals
  (is (thrown? clojure.lang.ExceptionInfo (keymap/parse-key-string "C-<up>"))))

(deftest bind-nests-prefixes-and-replaces-commands
  (let [a (fn [s] s)
        b (fn [s] s)
        s (-> {}
              (keymap/bind [:bindings] (keymap/parse-key-string "C-x") a)
              (keymap/bind [:bindings] (keymap/parse-key-string "C-x C-f") b))]
    (is (map? (get-in s [:bindings [:ctrl \x]])))
    (is (= b (get-in s [:bindings [:ctrl \x] [:ctrl \f]])))))