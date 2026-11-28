(ns nesmacs.benchmark-test
  "Roadmap M7 targets: open 10 MB under 2 s, keystroke on 100k lines under
   50 ms. These are timing assertions, so they can be noisy on a loaded box;
   the fixture sizes match the roadmap exactly."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [nesmacs.buf :as buf]
            [nesmacs.complete :as complete]
            [nesmacs.gap :as gap]
            [nesmacs.view :as view]
            [nesmacs.window :as win]))

(def ^:private line-count 100000)
(def ^:private bench-line
  ";; This is a test line for benchmarking performance with numbers 0123456789 and symbols foo bar baz qux")
(def ^:private large-text
  ;; Delayed so the 10 MB fixture is built only when a benchmark runs.
  (delay (str/join "\n" (repeat line-count bench-line))))

(defn- make-bench-state [text]
  {:buf "*bench*"
   :bufs {"*bench*" (buf/make "*bench*" text nil)}
   :msg nil :windows (win/leaf "*bench*") :cur-window []
   :rows 24 :cols 80
   :completion-sources {:default [complete/buffers complete/buffer-symbols]
                        :clojure-mode [complete/buffer-symbols complete/project-symbols]}})

(defn- render! [state]
  (view/render state)
  state)

(defn- type-char [state]
  (let [b (get-in state [:bufs "*bench*"])
        p (min (:point b) (max 0 (dec (count (:text b)))))
        b' (buf/edit b p p "x")]
    (-> state
        (assoc-in [:bufs "*bench*"] b')
        render!)))

(deftest ^:benchmark bench-fixture-matches-roadmap
  (is (= line-count (gap/line-count (:text (buf/make "*bench*" @large-text nil)))))
  (is (> (count @large-text) (* 10 1000 1000)) "fixture must be at least 10 MB"))

(deftest ^:benchmark bench-open-10mb
  (let [start (System/nanoTime)
        b (buf/make "*bench*" @large-text nil)
        elapsed (/ (- (System/nanoTime) start) 1e6)]
    (println (str "\n=== Open 10 MB: " (int elapsed) " ms (target < 2000) ==="))
    (is (:large-file b))
    (is (< elapsed 2000.0) "opening 10 MB should take < 2 s")))

(deftest ^:benchmark bench-keystroke-100k-lines
  (let [warm (-> (make-bench-state @large-text) render! type-char type-char)
        iterations 20
        start (System/nanoTime)
        _ (loop [i 0 s warm]
            (when (< i iterations) (recur (inc i) (type-char s))))
        elapsed (/ (- (System/nanoTime) start) 1e6)
        per-key (/ elapsed iterations)]
    (println (str "\n=== Keystroke on " line-count " lines: "
                  (float per-key) " ms (target < 50) ==="))
    (is (< per-key 50.0) "edit + render per keystroke should be < 50 ms")))
