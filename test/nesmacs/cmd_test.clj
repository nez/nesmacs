(ns nesmacs.cmd-test
  (:require [clojure.test :refer [deftest is]]
            [nesmacs.buf :as buf]
            [nesmacs.cmd :as cmd]
            [nesmacs.lang :as lang]
            [nesmacs.window :as win]))

(deftest a-window-point-is-clamped-to-the-buffer
  ;; Two windows on one buffer: the other window's edit can shrink the text
  ;; below this window's saved point. Rendering then crashed the editor.
  (let [s {:buf "a" :bufs {"a" (buf/make "a" "xy" nil)}
           :windows {:type :leaf :buffer "a" :point 99}
           :cur-window []}
        s (cmd/load-window-point s)]
    (is (= 2 (get-in s [:bufs "a" :point])))))

(deftest newline-keeps-space-only-indentation-on-the-new-line
  (let [s (-> {:buf "a"
               :bufs {"a" (assoc (buf/make "a" "(defn f []" nil)
                                  :language (lang/language :clojure))}
               :windows (win/leaf "a") :cur-window [] :rows 24 :cols 80}
              (cmd/goto-char 10)
              cmd/insert-newline)]
    (is (= "(defn f []\n  " (str (get-in s [:bufs "a" :text]))))
    (is (= 13 (get-in s [:bufs "a" :point])) "caret after the indent")))
