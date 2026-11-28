(ns nesmacs.treesit
  "Optional Tree-sitter backend: parse a buffer, run a vendored query and turn
   capture names into nesmacs faces.

   Nothing here is required for startup. If the native binding, a grammar or
   a query is missing, callers get nil and fall back to the line tokenizer.
   Parsing happens on the editor thread between keystrokes; the pre-existing
   large-file mode skips it entirely.

   Byte offsets come from Tree-sitter in UTF-8, so captures are converted to
   UTF-16 char offsets once per parse, using the text itself as the source of
   truth. Positions in the editor are char offsets."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [nesmacs.log :as log])
  (:import [org.treesitter TSParser TSLanguage TSTree TSQuery TSQueryCursor
            TSQueryMatch TSQueryCapture TSNode]))

(defonce ^:private grammars (atom {}))   ; class name -> TSLanguage | ::failed
(defonce ^:private queries (atom {}))    ; [queries kind] -> TSQuery | ::failed
(defonce ^:private states (atom {}))     ; buffer key -> parser/tree/spans cache

(def ^:dynamic *slow-parse-ms*
  "Latency budget for one parse, in milliseconds. Over budget the buffer stops
   parsing (tokenizer fallback) instead of freezing the editor. Dynamic so tests
   can force the fallback without a huge file."
  50)

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

(defn- collect-captures
  "Project query captures while the cursor is open, skipping nil results."
  [^TSQuery q ^TSTree tree ^String text capture-value]
  (let [cursor (TSQueryCursor.)
        match (TSQueryMatch.)]
    (try
      (.exec cursor q (.getRootNode tree) text)
      (loop [acc (transient [])]
        (if (.nextCapture cursor match)
          (let [value (capture-value (first (.getCaptures match)))]
            (recur (if (some? value) (conj! acc value) acc)))
          (persistent! acc)))
      (finally (.close cursor)))))

(defn- highlight-spans [^TSQuery q ^TSTree tree ^String text]
  (let [mapped (collect-captures q tree text
                 (fn [^TSQueryCapture capture]
                   (let [name (.getCaptureNameForId q (.getIndex capture))]
                     (when-let [face (capture-face name)]
                       (let [node (.getNode capture)
                             from (.getStartByte node)
                             to (.getEndByte node)]
                         (when (< from to)
                           [from to face (capture-priority name)]))))))
        char-map (byte->char text (mapcat (fn [[f t _ _]] [f t]) mapped))]
    (->> mapped
         (keep (fn [[from to face prio]]
                 (let [f (long (get char-map (long from)))
                       t (long (get char-map (long to)))]
                   (when (< f t) {:from f :to t :face face :priority prio}))))
         (sort-by (juxt :from :priority))
         vec)))

;; --- Folds ---

(defn- node-fold-range [^TSNode node]
  (let [start (long (.getRow (.getStartPoint node)))
        end-point (.getEndPoint node)
        end (long (.getRow end-point))
        end (if (zero? (.getColumn end-point)) (dec end) end)]
    (when (< start end) [start end])))

(defn- fold-ranges [^TSQuery q ^TSTree tree ^String text]
  (into [] (distinct)
        (collect-captures q tree text #(node-fold-range (.getNode ^TSQueryCapture %)))))

(def ^:private leaf-lit
  #{"sym_lit" "kwd_lit" "num_lit" "str_lit" "char_lit" "regex_lit"
    "sym_name" "kwd_name" "str_content" "comment"})

(defn- form-lit? [^String t]
  (and (.endsWith t "_lit") (not (contains? leaf-lit t))))

(defn- collect-lit-folds
  "Every nested Clojure form (`list_lit`, `vec_lit`, ...) spanning more than
   one line. The vendored folds query only captures top-level forms; folding
   the form at point needs the inner ones too."
  [^TSNode node acc]
  (let [acc (loop [i 0 acc acc]
              (if (< i (.getNamedChildCount node))
                (recur (inc i) (collect-lit-folds (.getNamedChild node i) acc))
                acc))]
    (if-let [range (when (form-lit? (.getType node)) (node-fold-range node))]
      (conj acc range)
      acc)))

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

(defonce ^:private states-lock (Object.))

(defn- compute*
  "Parse `text` for `key` and cache spans/folds for this version."
  [key l text version]
  (if-let [{:keys [parser]} (ensure-state key l)]
    (try
      (let [^String text (str text)
            old (:tree (parse-state key))
            t0 (System/nanoTime)
            tree (.parseString ^TSParser parser old text)
            spans (run-query l "highlights" tree text)
            folds (run-query l "folds" tree text)
            ms (/ (- (System/nanoTime) t0) 1e6)]
        (when old (try (.close ^TSTree old) (catch Throwable _)))
        (let [st (assoc (parse-state key) :tree tree :version version
                        :spans spans :folds folds)]
          (swap! states assoc key st)
          ;; Self-measured latency: once a parse costs more than the budget,
          ;; this buffer stops parsing and the tokenizer takes over for good
          ;; (like large-file mode). Measured, not guessed from line counts.
          (when (> ms (double *slow-parse-ms*))
            (log/log "tree-sitter slow parse:" (:id l) (str (long ms) "ms")
                     "- falling back to the tokenizer for" (str key))
            (swap! states update key assoc :slow? true :slow-ms (long ms)))
          st))
      (catch Throwable e
        ;; A native failure must not take the editor down; remember that this
        ;; language is unusable and let the caller tokenize instead.
        (log/log "tree-sitter parse failed:" (:id l) (.getMessage e))
        (swap! grammars assoc (:grammar l) ::failed)
        (swap! states dissoc key)
        nil))
    nil)
  ; DEBT: parsing runs on the editor thread between keystrokes. A parse slower
  ; than *slow-parse-ms* permanently falls back to the line tokenizer instead of
  ; freezing the editor; move to a worker with incremental parsing before v2 if
  ; a large buffer needs real highlighting.
  )

(defn- compute
  "Serialize parse/close: the web demo renders from a watch thread while the
   command loop renders, and both would otherwise parse for the same key,
   closing a native tree the other one is using."
  [key l text version]
  (locking states-lock
    (let [st (parse-state key)]
      (if (and st (= version (:version st)) (= (:lang-id st) (:id l)))
        st
        (compute* key l text version)))))

(defn state-for
  "Parsed state for `key` at `version`, parsing only when the version moved.
   nil once this buffer was marked slow, or when no grammar is available."
  [key l text version]
  (let [st (parse-state key)]
    (cond
      (:slow? st) nil
      (and st (= version (:version st)) (= (:lang-id st) (:id l))) st
      :else (compute key l text version))))

(defn slow?
  "True when parses for `key` exceeded the latency budget and the buffer fell
   back to the tokenizer. Cleared by `forget!` (buffer killed)."
  [key]
  (boolean (:slow? (parse-state key))))

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

(defn lit-folds
  "Nested form ranges from the parsed tree, for languages whose fold query is
   top-level only (Clojure). Nil when there is no parse."
  [key l text version]
  (when-let [st (state-for key l text version)]
    (locking states-lock
      ;; Another thread may have parsed a newer version (and closed this tree)
      ;; between state-for and the lock.
      (let [tree (:tree (parse-state key))]
        (when (and tree (= tree (:tree st)))
          (->> (collect-lit-folds (.getRootNode tree) []) distinct vec))))))


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

(defn- close-parse! [{:keys [tree parser]}]
  (when tree (try (.close ^TSTree tree) (catch Throwable _)))
  (when parser (try (.close ^TSParser parser) (catch Throwable _))))

(defn forget!
  "Release native memory for `key` (buffer killed)."
  [key]
  (locking states-lock
    (when-let [st (parse-state key)]
      (close-parse! st)
      (swap! states dissoc key))))

(defn reset-cache!
  "Test helper: drop parsers, trees and compiled queries."
  []
  (locking states-lock
    (run! close-parse! (vals @states))
    (reset! states {})
    (reset! queries {})
    (reset! grammars {})))
