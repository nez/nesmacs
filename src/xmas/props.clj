(ns xmas.props
  "Persistent face/property ranges on buffer text.

   Props are annotations that outlive a render: search matches, diagnostics,
   diff faces. Syntax highlighting is not stored here — `face.clj` computes it
   per line during rendering, and both are merged by the same span painter.

   A prop is {:from :to :face :priority ...extra}. Edits adjust ranges the same
   way markers do; zero-width props are preserved so caret-like uses survive.")

(defn make
  ([from to] (make from to :default {}))
  ([from to face] (make from to face {}))
  ([from to face props]
   (merge {:face face} props {:from from :to to})))

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
   dropped; props created empty (caret markers) are kept."
  [props e-from e-to repl-len]
  (->> props
       (keep (fn [p]
               (let [was-empty? (= (:from p) (:to p))
                     [f t] (adjust-range (:from p) (:to p) e-from e-to repl-len)]
                 (when (or was-empty? (< f t))
                   (assoc p :from f :to t)))))
       vec))

(defn in-range
  "Props that intersect the buffer range [from to)."
  [props from to]
  (filter (fn [p]
            (and (< (:from p) to) (> (:to p) from)))
          props))

(defn remove-owner
  "Drop every prop tagged with `owner` (e.g. :isearch), leaving other
   annotations untouched."
  [props owner]
  (filterv #(not= owner (:owner %)) props))

(defn put [props prop] (conj (vec props) prop))