(ns xmas.treesit
  "Optional Tree-sitter backend: parse a buffer, run a vendored query and turn
   capture names into xmas faces.

   Nothing here is required for startup. If the native binding, a grammar or
   a query is missing, callers get nil and fall back to the line tokenizer.
   Parsing happens on the editor thread between keystrokes; the pre-existing
   large-file mode skips it entirely.

   Byte offsets come from Tree-sitter in UTF-8, so captures are converted to
   UTF-16 char offsets once per parse, using the text itself as the source of
   truth. Positions in the editor are char offsets."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [xmas.log :as log])
  (:import [org.treesitter TSParser TSLanguage TSTree TSQuery TSQueryCursor
            TSQueryMatch TSQueryCapture TSNode]))

(defonce ^:private grammars (atom {}))   ; class name -> TSLanguage | ::failed
(defonce ^:private queries (atom {}))    ; [queries kind] -> TSQuery | ::failed
(defonce ^:private states (atom {}))     ; buffer key -> parser/tree/spans cache

;; --- Loading ---

(defn- instantiate
  "Construct a grammar without linking the class at namespace load time, so a
   missing grammar jar is a fallback rather than a startup error."
  [^String class-name]
  (try
    (let [cls (Class/forName class-name)
          ctor (.getDeclaredConstructor cls (make-array Class 0))]
      (.newInstance ctor (object-array 0)))
    (catch Throwable e
      (log/log "tree-sitter grammar unavailable:" class-name (.getMessage e))
      ::failed)))

(defn- grammar ^TSLanguage [class-name]
  (let [g (get @grammars class-name)]
    (if g
      (when-not (= ::failed g) g)
      (let [g (instantiate class-name)]
        (swap! grammars assoc class-name g)
        (when-not (= ::failed g) g)))))

(defn- resource-query [queries-dir kind]
  (when-let [r (io/resource (str "queries/" queries-dir "/" kind ".scm"))]
    (slurp r)))

(defn- query [l kind]
  (let [k [(:queries l) kind]
        q (get @queries k)]
    (if q
      (when-not (= ::failed q) q)
      (let [g (grammar (:grammar l))]
        (if-not g
          (do (swap! queries assoc k ::failed) nil)
          (let [src (resource-query (:queries l) kind)]
            (if (str/blank? src)
              (do (swap! queries assoc k ::failed) nil)
              (try (let [q (TSQuery. g src)]
                     (swap! queries assoc k q)
                     q)
                   (catch Throwable e
                     (log/log "tree-sitter query failed:" (:queries l) kind
                              (.getMessage e))
                     (swap! queries assoc k ::failed)
                     nil)))))))))

;; --- Captures -> faces ---

(defn- capture-face [^String cap]
  (cond
    (str/starts-with? cap "_") nil
    (str/starts-with? cap "comment") :comment
    (or (str/starts-with? cap "string")
        (str/starts-with? cap "character")
        (= cap "escape")) :string
    (or (str/starts-with? cap "keyword")
        (= cap "label")
        (= cap "attribute")) :keyword
    (str/starts-with? cap "function") :function
    (= cap "constructor") :function
    (or (str/starts-with? cap "type")
        (str/starts-with? cap "module")
        (= cap "tag")) :type
    (or (str/starts-with? cap "constant")
        (= cap "boolean")) :constant
    (str/starts-with? cap "number") :number
    (or (str/starts-with? cap "variable")
        (str/starts-with? cap "property")
        (str/starts-with? cap "field")) :variable
    (str/starts-with? cap "operator") :operator
    (str/starts-with? cap "builtin") :builtin
    :else nil))

(defn- capture-priority ^long [^String cap]
  (cond
    (str/starts-with? cap "comment") 100
    (str/starts-with? cap "string") 100
    (str/starts-with? cap "keyword") 60
    (str/starts-with? cap "function") 60
    (str/starts-with? cap "type") 60
    (str/starts-with? cap "constant") 60
    (str/starts-with? cap "number") 60
    (str/starts-with? cap "variable") 30
    (str/starts-with? cap "property") 30
    (str/starts-with? cap "field") 30
    (str/starts-with? cap "operator") 20
    :else 10))

(defn- utf8-width ^long [^long cp]
  (cond (< cp 0x80) 1 (< cp 0x800) 2 (< cp 0x10000) 3 :else 4))

(defn- byte->char
  "Map the requested UTF-8 byte offsets to UTF-16 char offsets in `s`.
   Offsets inside a multibyte char resolve to that char's start."
  [^String s targets]
  (let [targets (vec (sort (distinct targets)))
        n (.length s)]
    (loop [ci 0 bi 0 ts targets acc (transient {})]
      (if (empty? ts)
        (persistent! acc)
        (let [t (long (first ts))]
          (cond
            (= bi t) (recur ci bi (rest ts) (assoc! acc t ci))
            (>= ci n) (recur ci bi (rest ts) (assoc! acc t ci))
            :else
            (let [cp (.codePointAt s ci)]
              (recur (+ ci (Character/charCount cp))
                     (+ bi (utf8-width cp))
                     ts acc))))))))

(defn- captures
  "All captures of `q` over `tree` as [capture-name TSNode]."
  [^TSQuery q ^TSTree tree ^String text]
  (let [cursor (TSQueryCursor.)
        match (TSQueryMatch.)]
    (try
      (.exec cursor q (.getRootNode tree) text)
      (loop [acc (transient [])]
        (if (.nextCapture cursor match)
          (let [cap ^TSQueryCapture (first (.getCaptures match))
                node (.getNode cap)
                name (.getCaptureNameForId q (.getIndex cap))]
            (recur (conj! acc [name node])))
          (persistent! acc)))
      (finally (.close cursor)))))

(defn- highlight-spans [^TSQuery q ^TSTree tree ^String text]
  (let [mapped (keep (fn [[name ^TSNode node]]
                       (let [from (.getStartByte node)
                             to (.getEndByte node)]
                         (when-let [face (capture-face name)]
                           (when (< (long from) (long to))
                             [from to face (capture-priority name)]))))
                     (captures q tree text))
        char-map (byte->char text (mapcat (fn [[f t _ _]] [f t]) mapped))]
    (->> mapped
         (keep (fn [[from to face prio]]
                 (let [f (long (get char-map (long from)))
                       t (long (get char-map (long to)))]
                   (when (< f t) {:from f :to t :face face :priority prio}))))
         (sort-by (juxt :from :priority))
         vec)))

;; --- Folds ---

(defn- node-lines [^TSNode node]
  (let [sp (.getStartPoint node)
        ep (.getEndPoint node)]
    [(.getRow sp) (.getRow ep) (.getColumn ep)]))

(defn- fold-ranges [^TSQuery q ^TSTree tree ^String text]
  (let [cursor (TSQueryCursor.)
        match (TSQueryMatch.)]
    (try
      (.exec cursor q (.getRootNode tree) text)
      (loop [acc (transient [])]
        (if (.nextCapture cursor match)
          (let [cap ^TSQueryCapture (first (.getCaptures match))
                node (.getNode cap)
                [start-line end-line end-col] (node-lines node)
                end-line (if (zero? (long end-col)) (dec (long end-line)) (long end-line))]
            (recur (if (> end-line (long start-line))
                     (conj! acc [(long start-line) end-line])
                     acc)))
          (->> (persistent! acc) distinct vec)))
      (finally (.close cursor)))))

;; --- Per-buffer state ---

(defn- parse-state [key]
  (get @states key))

(defn- ensure-state [key l]
  (or (parse-state key)
      (let [g (grammar (:grammar l))
            st (when g {:parser (doto (TSParser.) (.setLanguage g))
                        :lang-id (:id l)})]
        (when st (swap! states assoc key st))
        st)))

(defn- run-query [l kind tree text]
  (when-let [q (query l kind)]
    (try
      (case kind
        "highlights" (highlight-spans q tree text)
        "folds" (fold-ranges q tree text))
      (catch Throwable e
        (log/log "tree-sitter query run failed:" (:queries l) kind
                 (.getMessage e))
        nil))))

(defn- compute
  "Parse `text` for `key` and cache spans/folds for this version."
  [key l text version]
  (if-let [{:keys [parser]} (ensure-state key l)]
    (try
      (let [^String text (str text)
            old (:tree (parse-state key))
            tree (.parseString ^TSParser parser old text)
            spans (run-query l "highlights" tree text)
            folds (run-query l "folds" tree text)]
        (when old (try (.close ^TSTree old) (catch Throwable _)))
        (let [st (assoc (parse-state key) :tree tree :version version
                        :spans spans :folds folds)]
          (swap! states assoc key st)
          st))
      (catch Throwable e
        ;; A native failure must not take the editor down; remember that this
        ;; language is unusable and let the caller tokenize instead.
        (log/log "tree-sitter parse failed:" (:id l) (.getMessage e))
        (swap! grammars assoc (:grammar l) ::failed)
        (swap! states dissoc key)
        nil))
    nil))

(defn state-for
  "Parsed state for `key` at `version`, parsing only when the version moved."
  [key l text version]
  (let [st (parse-state key)]
    (if (and st (= version (:version st)) (= (:lang-id st) (:id l)))
      st
      (compute key l text version))))

(defn spans
  "Highlight spans {:from :to :face :priority} for the whole buffer, or nil."
  [key l text version]
  (when-let [st (state-for key l text version)]
    (:spans st)))

(defn folds
  "Collapsed-range candidates [start-line end-line] for the whole buffer."
  [key l text version]
  (when-let [st (state-for key l text version)]
    (:folds st)))

(defn line-spans
  "Spans that overlap [from, to), using the sorted-by-from vector."
  [spans ^long from ^long to]
  (when (seq spans)
    (let [v (vec spans)
          n (count v)
          lo (loop [lo 0 hi n]
               (if (< lo hi)
                 (let [mid (quot (+ lo hi) 2)]
                   (if (< (long (:from (nth v mid))) from)
                     (recur (inc mid) hi)
                     (recur lo mid)))
                 lo))
          start (loop [i lo]
                  (if (and (pos? i)
                           (> (long (:to (nth v (dec i)))) from))
                    (recur (dec i))
                    i))]
      (loop [i start acc []]
        (if (and (< i n) (< (long (:from (nth v i))) to))
          (recur (inc i) (conj acc (nth v i)))
          acc)))))

(defn forget!
  "Release native memory for `key` (buffer killed)."
  [key]
  (when-let [st (get @states key)]
    (doseq [k [:tree :parser]]
      (when-let [o (get st k)]
        (try (.close o) (catch Throwable _))))
    (swap! states dissoc key)))

(defn reset-cache!
  "Test helper: drop parsers, trees and compiled queries."
  []
  (doseq [[_ st] @states]
    (doseq [k [:tree :parser]]
      (when-let [o (get st k)]
        (try (.close o) (catch Throwable _)))))
  (reset! states {})
  (reset! queries {})
  (reset! grammars {}))