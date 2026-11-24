(ns nesmacs.api-test
  (:require [clojure.test :refer [deftest is]]
            [nesmacs.api :as api]))

(defn- state []
  {:buf "a" :bufs {"a" {:point 0 :text ""}}
   :modes {} :hooks {} :commands {}})

(defn sample-command "Sample doc." [s] s)

(deftest bind-installs-handler-and-registers-command
  (let [a (atom (state))]
    (binding [api/*state* a]
      (api/bind! "C-c g" #'sample-command))
    (is (fn? (get-in @a [:bindings [:ctrl \c] \g])))
    (is (= "Sample doc." (get-in @a [:commands "sample-command" :doc])))
    (is (= @a ((get-in @a [:bindings [:ctrl \c] \g]) @a)))))

(deftest add-hook-accepts-keyword-or-symbol
  (let [a (atom (state))
        f (fn [s] s)]
    (binding [api/*state* a]
      (api/add-hook! :my-mode-hook f))
    (is (= [f] (get-in @a [:hooks 'my-mode-hook])))))

(deftest register-mode-parses-key-strings
  (let [a (atom (state))
        f (fn [s] s)]
    (binding [api/*state* a]
      (api/register-mode! :my-mode :major {"C-c C-c" f}))
    (is (= :major (get-in @a [:modes :my-mode :type])))
    (is (= f (get-in @a [:modes :my-mode :keymap [:ctrl \c] [:ctrl \c]])))))

(deftest configure-lsp-sets-command
  (let [a (atom (state))]
    (binding [api/*state* a]
      (api/configure-lsp! ["clangd" "--stdio"]))
    (is (= ["clangd" "--stdio"] (:lsp-command @a)))))

(deftest add-prop-installs-on-current-buffer
  (let [a (atom (assoc-in (state) [:bufs "a" :text] "abc"))]
    (binding [api/*state* a]
      (api/add-prop! 0 2 :match {:owner :demo :priority 5}))
    (let [[p] (get-in @a [:bufs "a" :props])]
      (is (= [0 2 :match 5 :demo]
             [(:from p) (:to p) (:face p) (:priority p) (:owner p)])))))

(deftest remove-prop-drops-only-that-owner
  (let [a (atom (assoc-in (state) [:bufs "a" :text] "abcd"))]
    (binding [api/*state* a]
      (api/add-prop! 0 2 :match {:owner :demo})
      (api/add-prop! 3 4 :diag-error {:owner :lsp})
      (api/remove-prop! :demo))
    (is (= [:lsp] (mapv :owner (get-in @a [:bufs "a" :props]))))))

(deftest set-prop-replaces-the-owners-range
  (let [a (atom (assoc-in (state) [:bufs "a" :text] "abcdef"))]
    (binding [api/*state* a]
      (api/set-prop! :hover 0 2 :match)
      (api/set-prop! :hover 3 5 :builtin))
    (let [ps (get-in @a [:bufs "a" :props])]
      (is (= 1 (count ps)))
      (is (= [3 5 :builtin :hover]
             [(:from (first ps)) (:to (first ps)) (:face (first ps)) (:owner (first ps))])))))

(deftest add-prop-clamps-range-to-buffer
  (let [a (atom (assoc-in (state) [:bufs "a" :text] "abc"))]
    (binding [api/*state* a]
      (api/add-prop! -5 99 :match))
    (let [[p] (get-in @a [:bufs "a" :props])]
      (is (= [0 3] [(:from p) (:to p)])))))

(deftest outside-config-context-throws
  (is (thrown? IllegalStateException (api/bind! "C-c x" identity))))