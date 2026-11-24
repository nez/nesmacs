(ns nesmacs.lang-test
  (:require [clojure.test :refer [deftest is testing]]
            [nesmacs.lang :as lang]))

(deftest extension-to-language
  (doseq [[file id] [["a.clj" "clojure"] ["a.cljc" "clojure"] ["a.edn" "clojure"]
                     ["a.py" "python"] ["a.pyi" "python"]
                     ["a.js" "javascript"] ["a.jsx" "javascript"]
                     ["a.ts" "typescript"] ["a.mts" "typescript"]
                     ["a.tsx" "tsx"]
                     ["a.el" "emacs-lisp"]]]
    (is (= id (:id (lang/language-for-file file))) file)))

(deftest unknown-and-missing-extensions-are-plain-text
  (doseq [f ["README.md" "Makefile" "noext" nil "" ".bashrc"]]
    (is (nil? (lang/language-for-file f)) (pr-str f))))

(deftest extension-match-is-case-insensitive
  (is (= "python" (:id (lang/language-for-file "APP.PY")))))

(deftest every-language-has-a-mode-and-grammar
  (doseq [[_ l] lang/languages]
    (is (keyword? (:mode l)))
    (is (string? (:comment-start l)))
    (testing "files with a configured server also carry argv"
      (is (or (nil? (:lsp l)) (vector? (:lsp l)))))))

(deftest mode-for-file-returns-nil-for-unknown
  (is (= :clojure-mode (lang/mode-for-file "x.clj")))
  (is (nil? (lang/mode-for-file "x.unknown"))))
(deftest debug-adapters-are-data-like-everything-else
  (testing "languages with an interactive debugger carry an adapter config"
    (doseq [id [:python :javascript :typescript :tsx]]
      (let [d (:dap (lang/language id))]
        (is (vector? (:argv d)) (str id))
        (is (contains? #{:stdio :tcp} (:transport d)) (str id)))))
  (testing "Clojure debugs through nREPL, not DAP"
    (is (nil? (:dap (lang/language :clojure))))))
