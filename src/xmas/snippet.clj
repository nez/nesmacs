(ns xmas.snippet
  "Minimal LSP snippet expansion: placeholders, defaults and choice lists.
   No tab-stop navigation; the caret lands on the first numbered stop.

   shortcut: nested placeholders, variables and mirrors are inserted as plain
   text. Extend when a server's snippets actually need them."
  (:require [clojure.string :as str]))

(def ^:private placeholder
  #"\$\{(\d+)(?::([^}]*))?\}|\$(\d+)")

(defn expand
  "Expand an LSP snippet body to plain text.
   Returns [text caret-offset]."
  [^String body]
  (let [body (str/replace body #"\$\{(\d+)\|([^}]*)\|\}"
                          (fn [[_ _ choices]] (first (str/split choices #","))))
        m (re-matcher placeholder body)
        matches (loop [acc []]
                  (if (.find m)
                    (recur (conj acc [(.start m) (.end m) (.group m 1) (.group m 2) (.group m 3)]))
                    acc))
        idx (fn [[_ _ n _ dollar]] (Long/parseLong (or n dollar)))
        tab (or (first (sort (filter pos? (map idx matches)))) 0)
        sb (StringBuilder.)]
    (loop [i 0 ms matches caret nil]
      (if-let [[s e _ default _ :as mt] (first ms)]
        (do
          (.append sb (subs body i s))
          (let [here (.length sb)
                caret (if (and (nil? caret) (= (idx mt) tab)) here caret)]
            (.append sb (or default ""))
            (recur e (rest ms) caret)))
        (do
          (.append sb (subs body i))
          [(str sb) (or caret (.length sb))])))))