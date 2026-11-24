(ns xmas.indent-test
  (:require [clojure.test :refer [deftest is testing]]
            [xmas.indent :as indent]
            [xmas.lang :as lang]
            [xmas.buf :as buf]
            [xmas.gap :as gap]))

(defn- b [lang-id text]
  (assoc (buf/make "t" text nil)
         :language (lang/language lang-id)))

(deftest clojure-indent-after-defn
  (let [text "(defn foo []\n"
        buf (b :clojure text)]
    (is (= "  " (indent/indent-line buf 1)))))

(deftest clojure-indent-after-let
  (let [text "(let [x 1]\n"
        buf (b :clojure text)]
    (is (= "  " (indent/indent-line buf 1)))))

(deftest clojure-indent-body-of-defn
  (let [text "(defn foo []\n  (let [x 1]\n"
        buf (b :clojure text)]
    (is (= "    " (indent/indent-line buf 2)))))

(deftest clojure-indent-nested-let
  (let [text "(defn foo []\n  (let [x 1]\n    (let [y 2]\n"
        buf (b :clojure text)]
    (is (= "      " (indent/indent-line buf 3)))))

(deftest python-indent-after-colon
  (let [text "def foo():\n"
        buf (b :python text)]
    (is (= "    " (indent/indent-line buf 1)))))

(deftest python-indent-dedent-on-return
  (let [text "def foo():\n    x = 1\n    return x\n"
        buf (b :python text)]
    (is (= "    " (indent/indent-line buf 2)))  ;; inside function
    (is (= "" (indent/indent-line buf 3)))))    ;; after return, dedent

(deftest python-indent-copy-previous
  (let [text "if x:\n    y = 1\n"
        buf (b :python text)]
    (is (= "    " (indent/indent-line buf 2)))))

(deftest javascript-indent-after-brace
  (let [text "function foo() {\n"
        buf (b :javascript text)]
    (is (= "  " (indent/indent-line buf 1)))))

(deftest javascript-indent-copy-previous
  (let [text "if (x) {\n  y = 1;\n"
        buf (b :javascript text)]
    (is (= "  " (indent/indent-line buf 2)))))

(deftest unknown-language-no-indent
  (let [text "plain\n"
        buf (assoc (buf/make "t" text nil) :mode :fundamental :language nil)]
    (is (nil? (indent/indent-line buf 1)))))