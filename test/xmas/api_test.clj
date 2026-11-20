(ns xmas.api-test
  (:require [clojure.test :refer [deftest is]]
            [xmas.api :as api]))

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

(deftest outside-config-context-throws
  (is (thrown? IllegalStateException (api/bind! "C-c x" identity))))