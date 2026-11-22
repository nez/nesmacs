(ns xmas.view-test
  (:require [clojure.test :refer [deftest is]]
            [xmas.buf :as buf]
            [xmas.props :as props]
            [xmas.term :as term]
            [xmas.view :as view]
            [xmas.window :as win]))

;; --- apply-props: paint buffer props onto line-local spans ---

(deftest apply-props-without-props
  (is (= [[0 5 :default]] (view/apply-props [[0 5 :default]] [] 0 5))))

(deftest apply-props-clamps-to-the-line
  ;; Prop [8 12) paints only its intersection with line [10 15).
  (is (= [[0 2 :match] [2 5 :default]]
         (view/apply-props [[0 5 :default]]
                           [(props/make 8 12 :match)] 10 5))))

(deftest apply-props-sorts-by-priority-not-insertion
  ;; The diagnostic is inserted first but has higher priority, so it paints
  ;; last and wins on the overlap [2 5). The split at the match boundary is
  ;; expected because apply-face-range doesn't merge adjacent same-face spans.
  (let [ps [(props/make 2 5 :diag-error {:priority 10})
            (props/make 0 3 :match)]
        out (view/apply-props [[0 5 :default]] ps 0 5)]
    (is (= :match (get (nth out 0) 2)))
    (is (= :diag-error (get (nth out 1) 2)))
    (is (= :diag-error (get (nth out 2) 2)))))

(deftest apply-props-region-beats-diagnostics
  (let [ps [(props/make 0 5 :region {:priority 100})
            (props/make 0 5 :diag-error {:owner :lsp})]]
    (is (= [[0 5 :region]] (view/apply-props [[0 5 :default]] ps 0 5)))))

;; --- apply-face-range directly ---

(deftest apply-face-range-general-face
  (is (= [[0 2 :default] [2 5 :builtin] [5 10 :default]]
         (view/apply-face-range [[0 10 :default]] 2 5 :builtin))))

;; --- region is painted through props ---

(defn- render-str [s]
  (let [out (StringBuilder.)]
    (binding [term/*out-fn* #(.append out (str %))]
      (view/render s))
    (str out)))

;; :region is bg 24 in view/faces → SGR "48;5;24".
(def ^:private region-sgr "48;5;24")

(deftest render-paints-region-through-props
  (let [b (assoc (buf/make "t" "abcdef" nil) :mark 1 :point 4)
        s {:buf "t" :bufs {"t" b}
           :windows (win/leaf "t") :cur-window []
           :rows 5 :cols 20}]
    (is (.contains (render-str s) region-sgr))))

(deftest render-paints-region-in-large-file-mode
  ;; Large-file mode skips fontification and buffer props, but the region is a
  ;; render-time prop and must still show.
  (let [b (assoc (buf/make "t" "abcdef" nil)
                 :mark 1 :point 4 :large-file true)
        s {:buf "t" :bufs {"t" b}
           :windows (win/leaf "t") :cur-window []
           :rows 5 :cols 20}]
    (is (.contains (render-str s) region-sgr))))

(deftest render-does-not-paint-an-empty-region
  (let [b (assoc (buf/make "t" "abcdef" nil) :mark 2 :point 2)
        s {:buf "t" :bufs {"t" b}
           :windows (win/leaf "t") :cur-window []
           :rows 5 :cols 20}]
    (is (not (.contains (render-str s) region-sgr)))))

;; --- font-lock cache: fontification props are computed once per line ---

(defn- render-result [s]
  (binding [term/*out-fn* (fn [_])]
    (view/render s)))

(deftest render-reuses-cached-font-lock-spans
  ;; A sentinel :builtin span in the cache must survive: if the renderer
  ;; re-tokenized "(abcdef" it would paint :default instead.
  (let [b (-> (buf/make "t" "abcdef" nil)
              (assoc :mode :clojure-mode
                     :font-lock [[[[0 6 :builtin]] :normal]]))
        s {:buf "t" :bufs {"t" b}
           :windows (win/leaf "t") :cur-window []
           :rows 5 :cols 20}]
    ;; :builtin is fg 67 in view/faces.
    (is (.contains (render-str s) "38;5;67"))))

(deftest render-fills-font-lock-cache
  (let [b (assoc (buf/make "t" "(defn foo [])" nil) :mode :clojure-mode)
        s {:buf "t" :bufs {"t" b}
           :windows (win/leaf "t") :cur-window []
           :rows 5 :cols 20}
        {:keys [font-lock-updates]} (render-result s)
        [spans state] (first (get-in font-lock-updates ["t" :font-lock]))]
    (is (seq spans))
    (is (= :normal state))))

(deftest render-large-file-skips-font-lock
  (let [b (assoc (buf/make "t" "(defn foo [])" nil)
                 :mode :clojure-mode :large-file true)
        s {:buf "t" :bufs {"t" b}
           :windows (win/leaf "t") :cur-window []
           :rows 5 :cols 20}
        {:keys [font-lock-updates]} (render-result s)]
    (is (= [] (get-in font-lock-updates ["t" :font-lock])))))

;; --- render with a 0-body-row pane must not crash ---

(deftest render-tiny-split-doesnt-crash
  ;; Regression: a stacked split of a 3-row frame leaves the pane with a
  ;; 1-row rect, i.e. body-rows = 0. ensure-visible used to unconditionally
  ;; compute nth-line-start of (inc point-line) in that branch, which
  ;; blew up with AIOOB for a point on the last line.
  (let [b (assoc (buf/make "t" "a\nb\nc\nd\ne" nil) :point 8)
        [tree path] (win/split (win/leaf "t") [] :stacked)
        s {:buf "t" :bufs {"t" b}
           :windows tree :cur-window path
           :rows 3 :cols 10}]
    (let [out (StringBuilder.)]
      (binding [xmas.term/*out-fn* #(.append out (str %))]
        (is (map? (view/render s))))
      (is (not (.contains (str out) (str (char 27) "[0;1H")))))))
