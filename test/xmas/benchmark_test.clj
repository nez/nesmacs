(ns xmas.benchmark-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [xmas.buf :as buf]
            [xmas.gap :as gap]
            [xmas.view :as view]
            [xmas.window :as win]
            [xmas.complete :as complete]))

;; Benchmark tests - these verify performance characteristics but don't fail on CI.
;; They print timing info for manual inspection.
;; Roadmap targets: open 10 MB < 2 s; keystroke on 100k lines < 50 ms.

(def ^:private large-text (str/join "\n" (repeat 50000 ";; This is a test line for benchmarking\n")))

(defn- make-bench-state [text]
  {:buf "*bench*"
   :bufs {"*bench*" (buf/make "*bench*" text nil)}
   :msg nil :windows (win/leaf "*bench*") :cur-window []
   :rows 24 :cols 80
   :completion-sources {:default [complete/buffers complete/buffer-symbols]
                        :clojure-mode [complete/buffer-symbols complete/project-symbols]}})

(defn- render-loop [state n]
  (loop [i 0 s state]
    (if (< i n)
      (let [snapshot s
            {:keys [sig line-state-updates]} (view/render snapshot)
            wp (get-in snapshot [:windows :cur-window] [])
            updated (-> snapshot
                        (assoc :render-sig sig)
                        (update-in (conj wp :scroll) (fn [_] 0)))]
        (recur (inc i) updated))
      s)))

(deftest bench-large-file-render
  (let [state (make-bench-state large-text)
        iterations 100
        text (:text (get-in state [:bufs "*bench*"]))
        lines (gap/line-count text)]
    (println "\n=== Large File Render Benchmark ===")
    (println (str "Lines: " lines))
    (println (str "Chars: " (count (str text))))
    (let [start (System/nanoTime)
          _ (render-loop state iterations)
          elapsed (/ (- (System/nanoTime) start) 1e6)]
      (println (str "Render " iterations " frames: " (int elapsed) " ms"))
      (println (str "Avg per frame: " (float (/ elapsed iterations)) " ms"))
      (is (< (/ elapsed iterations) 50.0) "Avg frame time should be < 50ms"))))

(deftest bench-large-file-open
  (println "\n=== Large File Open Benchmark ===")
  (let [start (System/nanoTime)
        b (buf/make "*bench*" large-text nil)
        elapsed (/ (- (System/nanoTime) start) 1e6)]
    (println (str "Buffer creation: " (int elapsed) " ms"))
    (is (< elapsed 2000.0) "Buffer creation should be < 2000ms (roadmap: 10MB < 2s)"))
  ;; Pre-create buffer for edit tests
  (let [b (buf/make "*bench*" large-text nil)]
    ;; Edit at end (typing at point) - more realistic
    (let [start (System/nanoTime)
          _ (buf/set-point b (fn [_ _] (count (:text b))))  ;; move to end
          _ (buf/edit b (count (:text b)) (count (:text b)) ";; appended\n")
          elapsed (/ (- (System/nanoTime) start) 1e6)]
      (println (str "Edit at end: " (int elapsed) " ms"))
      (is (< elapsed 100.0) "Edit at end should be < 100ms"))
    ;; Edit at middle - also realistic
    (let [start (System/nanoTime)
          mid (quot (count (:text b)) 2)
          _ (buf/set-point b (fn [_ _] mid))
          _ (buf/edit b mid mid ";; middle\n")
          elapsed (/ (- (System/nanoTime) start) 1e6)]
      (println (str "Edit at middle: " (int elapsed) " ms"))
      (is (< elapsed 100.0) "Edit at middle should be < 100ms"))))

;; Run with: clojure -M:test -n xmas.benchmark-test
;; Or run individually: clojure -M:test -v xmas.benchmark-test/bench-large-file-render