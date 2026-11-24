(ns nesmacs.nav-test
  (:require [clojure.test :refer [deftest is]]
            [nesmacs.gap :as gap]
            [nesmacs.nav :as nav]))

(deftest occur-entries-find-every-match-line
  (let [es (nav/occur-entries (gap/of "alpha\nbeta alpha\ngamma") "alpha")]
    (is (= [1 2] (mapv :line es)))
    (is (= ["alpha" "beta alpha"] (mapv :text es)))))

(deftest occur-entries-are-capped
  ;; A pattern matching every char on every line still stops at the cap.
  (let [text (gap/of (apply str (repeat 3000 "x\n")))]
    (is (<= (count (nav/occur-entries text "x")) 2000))))

(deftest imenu-symbols-reads-definitions
  (let [text "(ns a)\n\n(defn foo [] 1)\n(def x 2)\n(defn- bar\n  :ok)\n"
        syms (nav/imenu-symbols {:mode :clojure-mode} text)]
    (is (= [["foo" 3] ["x" 4] ["bar" 5]]
           (mapv (juxt :name :line) syms)))))

(deftest imenu-symbols-ignores-non-lisp-modes
  (is (empty? (nav/imenu-symbols {:mode :fundamental} "(defn foo [] 1)"))))

(deftest rg-line-parses-results
  (is (= {:file "src/a.clj" :line 12}
         (nav/rg-line "src/a.clj:12:(defn foo [] 1)")))
  (is (nil? (nav/rg-line "no match here"))))