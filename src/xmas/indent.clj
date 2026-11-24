(ns xmas.indent
  "Minimal per-language indentation for RET. Each mode gets a function
   (text line) -> indent-string, where `line` is the 0-based line number
   the cursor is on AFTER the newline was inserted. The function decides
   what the new line's indentation should be."
  (:require [clojure.string :as str]))

(def ^:private lisp-indent-specials
  #{"defn" "defn-" "def" "defmacro" "defmulti" "defmethod" "defprotocol"
    "defrecord" "deftype" "defonce" "deftest" "defspec"
    "fn" "let" "let*" "letfn" "loop" "if" "if-let" "when" "when-let" "cond"
    "case" "do" "try" "catch" "finally" "->" "->>" "some->" "some->>"
    "as->" "cond->" "cond->>" "for" "doseq" "dotimes" "with-open" "binding"})

(defn- line-start-pos [text line]
  (if (= line 0)
    0
    (loop [l 0 pos 0 lines (str/split-lines text)]
      (if (>= l line)
        pos
        (let [ln (first lines)]
          (recur (inc l) (+ pos (count ln) 1) (rest lines)))))))

(defn- form-line-col
  "Column of position `pos` on its line (0-based)."
  [text pos]
  (loop [p (dec pos) col 0]
    (cond
      (< p 0) col
      (= (.charAt text p) \newline) col
      :else (recur (dec p) (inc col)))))

(defn- find-form-start
  "Find the position of the opening paren of the form containing `pos`."
  [text pos]
  (loop [i (dec pos) depth 0 n (count text)]
    (cond
      (< i 0) nil
      (= (.charAt text i) \)) (recur (dec i) (inc depth) n)
      (= (.charAt text i) \()
      (if (zero? depth)
        i
        (recur (dec i) (dec depth) n))
      (Character/isWhitespace (.charAt text i)) (recur (dec i) depth n)
      :else (recur (dec i) depth n))))

(defn- first-symbol
  "Return [sym-start sym-end] of the first symbol after `start`, or nil."
  [text start]
  (let [n (count text)]
    (loop [j (inc start)]
      (when (< j n)
        (let [ch (.charAt text j)]
          (cond
            (Character/isWhitespace ch) (recur (inc j))
            (= ch \") (recur (inc j))
            (= ch \;) (loop [k j]
                        (if (or (>= k n) (= (.charAt text k) \newline))
                          (recur (inc k))
                          k))
            :else (let [sym-start j
                        sym-end (loop [k j]
                                  (if (or (>= k n)
                                          (Character/isWhitespace (.charAt text k))
                                          (some #{ch} [\( \) \" \;]))
                                    k
                                    (recur (inc k))))]
                    [sym-start sym-end])))))))

(defn- lisp-indent [^String text ^long line]
  (let [start (line-start-pos text line)
        form-start (find-form-start text (dec start))]
    (if (nil? form-start)
      ""
      (let [col (form-line-col text form-start)
            sym (first-symbol text form-start)]
        (if (and sym
                 (lisp-indent-specials (subs text (first sym) (second sym))))
          (str (apply str (repeat (+ col 2) " ")))
          (str (apply str (repeat (+ col 2) " "))))))))

(defn- prev-non-empty-line [lines line]
  (loop [l (dec line)]
    (if (>= l 0)
      (if (str/blank? (get lines l))
        (recur (dec l))
        l)
      nil)))

(defn- python-indent [^String text ^long line]
  (let [lines (str/split-lines text)
        prev-line (prev-non-empty-line lines line)]
    (if (nil? prev-line)
      ""
      (let [prev-text (get lines prev-line)
            base-indent (or (second (re-find #"^(\s*)" prev-text)) "")
            trimmed (str/trim prev-text)]
        (cond
          (str/ends-with? trimmed ":") (str base-indent "    ")
          (re-find #"^\s*(return|raise|break|continue|pass|else:|elif |except|finally:)" trimmed)
          (if (>= (count base-indent) 4) (subs base-indent 4) "")
          (re-find #"^\s*(def |class |with |for |while |if |elif |else:|try:|except |finally:|match |case )" trimmed)
          (str base-indent "    ")
          :else base-indent)))))

(defn- c-like-indent [^String text ^long line]
  (let [lines (str/split-lines text)
        prev-line (prev-non-empty-line lines line)]
    (if (nil? prev-line)
      ""
      (let [prev-text (get lines prev-line)
            base-indent (or (second (re-find #"^(\s*)" prev-text)) "")
            trimmed (str/trim prev-text)]
        (if (or (str/ends-with? trimmed "{") (str/ends-with? trimmed ":"))
          (str base-indent "  ")
          base-indent)))))

(defmulti indent-for
  "Return the indentation string for a new line at `line` in `text`
   for language `l`."
  (fn [l _text _line] (:id l)))

(defmethod indent-for "clojure" [_l text line] (lisp-indent text line))
(defmethod indent-for "python" [_l text line] (python-indent text line))
(defmethod indent-for "javascript" [_l text line] (c-like-indent text line))
(defmethod indent-for "typescript" [_l text line] (c-like-indent text line))
(defmethod indent-for "tsx" [_l text line] (c-like-indent text line))
(defmethod indent-for :default [_l _text _line] "")

(defn indent-line
  "Indentation for a new line at `line` (0-based) in `buf`'s text."
  [buf line]
  (when-let [l (:language buf)]
    (indent-for l (str (:text buf)) line)))