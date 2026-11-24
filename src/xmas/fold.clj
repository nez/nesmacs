(ns xmas.fold
  "Line folding. A fold is [start-line end-line]; the start line stays visible
   with an ellipsis and the rest is skipped while rendering.

   Ranges come from the Tree-sitter `folds.scm` query. When the grammar is not
   available, Lisp buffers fall back to paren depth, which is what actually
   defines a form there."
  (:require [xmas.gap :as gap]
            [xmas.treesit :as treesit]))

(defn- lisp-ranges
  "Top-level forms spanning more than one line, by paren depth."
  [^CharSequence text]
  (let [n (count text)]
    (loop [i 0 depth 0 start 0 acc []]
      (if (>= i n)
        acc
        (let [c (.charAt text i)]
          (cond
            (= c \()
            (if (zero? depth)
              (recur (inc i) 1 i acc)
              (recur (inc i) (inc depth) start acc))

            (= c \))
            (cond
              (> depth 1) (recur (inc i) (dec depth) start acc)
              (= depth 1)
              (let [s (gap/line-of text start)
                    e (gap/line-of text i)]
                (recur (inc i) 0 0 (if (> e s) (conj acc [s e]) acc)))
              :else (recur (inc i) 0 start acc))

            :else (recur (inc i) depth start acc)))))))

(defn ranges
  "Candidate fold ranges for `buf`, sorted and de-duplicated."
  [buf-key buf text version]
  (let [l (:language buf)
        lisp? (#{:clojure-mode :emacs-lisp-mode} (:mode buf))
        ts (when l (if (= "clojure" (:id l))
                      (treesit/lit-folds buf-key l text version)
                      (treesit/folds buf-key l text version)))
        rs (or ts (when lisp? (lisp-ranges text)))]
    (->> (or rs [])
         (filter (fn [[s e]] (> (long e) (long s))))
         (sort-by first)
         (vec))))

;; --- Display math ---

(defn- covering
  "The collapsed range hiding `line` (start < line <= end), if any. The fold's
   start line stays visible and carries the ellipsis."
  [folds ^long line]
  (some (fn [[s e]] (when (and (< (long s) line) (<= line (long e))) [s e])) folds))

(defn hidden?
  [folds line]
  (boolean (covering folds (long line))))

(defn- hidden-before ^long [folds ^long line]
  (reduce (fn [n [s e]]
            (let [hi (min (long e) (dec line))]
              (if (< (long s) hi) (+ n (- hi (long s))) n)))
          0 folds))

(defn visible-before
  "Number of visible lines before buffer line `line`."
  ^long [folds line]
  (- (long line) (hidden-before folds (long line))))

(defn visible-count
  ^long [folds total]
  (- (long total)
     (reduce (fn [n [s e]] (+ n (- (long e) (long s)))) 0 folds)))

(defn line-at-display
  "Buffer line shown at display index `d`."
  ^long [folds ^long d]
  (loop [line d rs folds]
    (if-let [[s e] (first rs)]
      (if (< (long s) line)
        (recur (+ line (- (long e) (long s))) (rest rs))
        line)
      line)))

(defn visible-start
  "Scroll target line: never the inside of a folded range."
  ^long [folds ^long line]
  (if-let [[s _] (covering folds line)] (long s) line))

(defn effective
  "Folds to apply while `line` must stay visible (point or scroll there)."
  [folds ^long line]
  (filterv (fn [[s e]] (not (and (< (long s) line) (<= line (long e))))) folds))

(defn toggle
  "Collapse or expand the innermost range starting at or containing `line`."
  [ranges collapsed ^long line]
  (let [rs (->> ranges
                (filter (fn [[s e]] (and (<= (long s) line) (<= line (long e)))))
                (sort-by (fn [[s e]] (- (long e) (long s)))))
        [s _] (first rs)]
    (if-not s
      collapsed
      (if (contains? collapsed s)
        (disj collapsed s)
        (conj collapsed s)))))