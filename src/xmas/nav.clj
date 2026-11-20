(ns xmas.nav
  "Location listings that jump elsewhere: occur (regex hits in one buffer),
   imenu (definitions read from the tokenizer), and ripgrep result lines.

   The tokenizer has no parser behind it, so imenu only sees definitions whose
   keyword and name sit on one line — good enough for Clojure and Lisp."
  (:require [clojure.string :as str]
            [xmas.face :as face]
            [xmas.gap :as gap]
            [xmas.listbuf :as listbuf]))

;; --- Occur ---

(def ^:private max-occur 2000)

(defn occur-entries
  "One entry per regex match in `text`, capped so a pathological pattern can't
   build an unbounded list."
  [^String text pattern]
  (let [m (re-matcher (re-pattern pattern) text)]
    (loop [acc []]
      (if (or (>= (count acc) max-occur) (not (.find m)))
        acc
        (let [s  (.start m)
              ln (gap/line-of text s)
              ls (gap/nth-line-start text ln)
              le (gap/nth-line-end text ln)]
          (recur (conj acc {:line (inc ln) :text (gap/substr text ls le)})))))))

(defn occur-buffer [name source pattern entries]
  (listbuf/make
    {:name         name
     :mode         :nav-mode
     :header       (str (count entries) " matches for " pattern)
     :entries      (mapv #(assoc % :buf source) entries)
     :format-entry (fn [{:keys [line text]}] (format "%6d: %s" line text))
     :extras       {:occur-source source}}))

;; --- Imenu ---

(def ^:private def-keywords
  #{"defn" "defn-" "def" "defmacro" "defmulti" "defmethod" "defprotocol"
    "defrecord" "deftype" "defonce" "deftest" "defspec"
    "defun" "defvar" "defconst" "defcustom" "defgroup" "defsubst"
    "define-derived-mode" "define-minor-mode"})

(defn- symbol-after
  "First symbol on `line` at or after `from`, skipping whitespace, opening
   parens and quote characters."
  [^String line ^long from]
  (let [n (.length line)
        i (loop [i from]
            (if (and (< i n)
                     (let [c (.charAt line i)]
                       (or (Character/isWhitespace c) (= c \() (= c \'))))
              (recur (inc i)) i))]
    (when (< i n)
      (let [end (loop [j i]
                  (if (and (< j n)
                           (not (Character/isWhitespace (.charAt line j)))
                           (not (contains? #{\( \) \[ \] \{ \} \, \" \;}
                                           (.charAt line j))))
                    (recur (inc j)) j))]
        (subs line i end)))))

(defn imenu-symbols
  [{:keys [mode]} ^String text]
  (->> (str/split-lines text)
       (keep-indexed
         (fn [i line]
           (let [[toks _] (face/tokenize mode line :normal)]
             (when-let [name (some (fn [[from to f]]
                                     (when (and (= :builtin f)
                                                (contains? def-keywords
                                                           (subs line from to)))
                                       (symbol-after line to)))
                                   toks)]
               {:line (inc i) :name name}))))
       vec))

(defn imenu-buffer [name source symbols]
  (listbuf/make
    {:name         name
     :mode         :nav-mode
     :header       (str (count symbols) " symbols")
     :entries      (mapv #(assoc % :buf source) symbols)
     :format-entry (fn [{:keys [name line]}]
                     (format "%5d  %s" line name))
     :extras       {:occur-source source}}))

;; --- Ripgrep output ---

(defn rg-line
  "Parse `path:line:text` (rg with --with-filename). Nil for other lines."
  [^String line]
  (when-let [[_ file ln] (re-find #"^([^:]+):(\d+):" line)]
    {:file file :line (Long/parseLong ln)}))