(ns xmas.props
  "Persistent face/property ranges on buffer text.

   Props are durable annotations that outlive a render and follow edits:
   search matches, LSP diagnostics, diff faces, and the region (created at
   render time, never stored). Syntax highlighting is deliberately not stored
   here — `face.clj` tokenizes the visible lines and the buffer caches the
   per-line spans and lexer states in `:font-lock`, because every edit would
   otherwise walk every token in the file. Both layers are painted by the same
   span painter in `view.clj`.

   A prop is {:from :to :face :priority ...extra}. Higher :priority paints
   later and wins visually; ties keep insertion order. Edits adjust ranges the
   same way markers do; zero-width props are preserved so caret-like uses
   survive.")

(defn make
  ([from to] (make from to :default {}))
  ([from to face] (make from to face {}))
  ([from to face props]
   (merge {:face face :priority 0} props {:from from :to to})))

(defn- adjust-range
  "Shift/shrink a single [from to] range to reflect an edit at [e-from e-to)
   being replaced with text of length `repl-len`."
  [from to e-from e-to repl-len]
  (let [delta (- repl-len (- e-to e-from))]
    (cond
      ;; entirely before the edit — unchanged
      (<= to e-from) [from to]
      ;; entirely after the edit — shift by delta
      (>= from e-to) [(+ from delta) (+ to delta)]
      ;; edit fully contains the prop — collapse at edit start
      (and (<= e-from from) (>= e-to to)) [e-from e-from]
      ;; edit overlaps the start
      (and (<= e-from from) (< e-to to)) [e-from (+ to delta)]
      ;; edit overlaps the end
      (and (> e-from from) (>= e-to to)) [from e-from]
      ;; edit is fully inside the prop — stretch
      :else [from (+ to delta)])))

(defn adjust
  "Update prop positions after an edit at [e-from e-to) replaced with
   `repl-len` characters. Props that collapsed as a result of this edit are
   dropped; props created empty (caret markers) are kept. Source order is
   preserved: adjust-range is monotone in both endpoints."
  [props e-from e-to repl-len]
  (->> props
       (keep (fn [p]
               (let [was-empty? (= (:from p) (:to p))
                     [f t] (adjust-range (:from p) (:to p) e-from e-to repl-len)]
                 (when (or was-empty? (< f t))
                   (assoc p :from f :to t)))))
       vec))

(defn in-range
  "Props that intersect the buffer range [from to). Expected to be sparse:
   this filters the whole vector, which is fine for annotations and is why
   fontification does not live here."
  [props from to]
  (filterv (fn [p]
             (and (< (:from p) to) (> (:to p) from)))
           props))

(defn remove-owner
  "Drop every prop tagged with `owner` (e.g. :isearch), leaving other
   annotations untouched."
  [props owner]
  (filterv #(not= owner (:owner %)) props))

(defn- by-position [p] [(:from p) (:to p)])

(defn put
  "Add one prop, keeping the vector sorted by position so painting and tests
   have a stable order independent of owner churn."
  [props prop]
  (vec (sort-by by-position (conj (vec props) prop))))

(defn add-all
  "Add many props at once (e.g. a diagnostics batch)."
  [props new-props]
  (vec (sort-by by-position (into (vec props) new-props))))

(defn set-owner
  "Replace every prop owned by `owner` with one range. Use this for a single
   logical annotation (current match, hover target); use `put`/`add-all` to
   accumulate several ranges under one owner."
  [props owner from to face opts]
  (put (remove-owner props owner)
       (make from to face (assoc opts :owner owner))))
