(ns ^:integration xmas.dap-live-test
  "End-to-end check against the real debugpy DAP adapter. Excluded from
   `make test` (it needs Python and debugpy); run `make dap`.

   Set XMAS_DEBUGPY to the Python executable that has debugpy installed."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [xmas.buf :as buf]
            [xmas.dap :as dap]
            [xmas.lang :as lang]))

(defn- python-with-debugpy []
  (let [py (or (System/getenv "XMAS_DEBUGPY") "python3")]
    (when (zero? (:exit (shell/sh py "-c" "import debugpy")))
      py)))

(defn- pump!
  "Tick the DAP client until `pred` holds or the deadline passes."
  [editor pred ms]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop []
      (dap/tick! editor)
      (cond (pred) true
            (> (System/currentTimeMillis) end) false
            :else (do (Thread/sleep 50) (recur))))))

(defn- text-of [editor name]
  (some-> (get-in @editor [:bufs name :text]) str))

(deftest debugpy-session-hits-a-breakpoint-and-reports-state
  (if-let [py (python-with-debugpy)]
    (let [dir (doto (java.io.File/createTempFile "xmas-dap-live" "")
                (.delete) (.mkdirs))
          prog (java.io.File. dir "prog.py")
          _ (spit prog "def add(a, b):\n    total = a + b\n    return total\n\nprint(add(2, 3))\n")
          path (.getPath prog)
          editor (atom {:bufs {path (buf/make path (slurp prog) path)}
                        :msg nil :rows 24 :cols 80})
          stop (atom nil)
          language (lang/language :python)
          cfg (assoc-in (:dap language) [:argv] [py "-m" "debugpy.adapter"])]
      (try
        (dap/restore-breakpoints! {})
        (dap/set-breakpoints! path [2])          ; 0-based: `return total`
        (is (re-find #"Starting python debugger"
                     (dap/start! {:editor editor :root (.getPath dir)
                                  :file path :language language :dap cfg
                                  :on-stop (fn [s frame] (reset! stop frame) s)})))
        (is (pump! editor #(= :stopped (dap/state)) 20000) "the breakpoint is hit")
        (is (pump! editor #(some? (get-in @editor [:bufs "*Debug*"])) 5000))
        (is (pump! editor #(some? (get-in @editor [:bufs "*Debug Locals*"])) 5000))

        (is (= {:file path :line 2 :id (:id @stop)}
               (select-keys @stop [:file :line :id]))
            "on-stop points at the source line")
        (is (str/includes? (text-of editor "*Debug*") "add"))
        (let [locals (text-of editor "*Debug Locals*")]
          (is (str/includes? locals "total"))
          (is (str/includes? locals "= 5")))
        (is (= {:width 1 :marks {2 {:text "▶" :face :debug-stop}}}
               (get-in @editor [:bufs path :gutter]))
            "the stopped line gets the arrow")

        (dap/evaluate! "a + b")
        (is (pump! editor #(re-find #"=> 5" (str (:msg @editor))) 5000)
            "evaluate runs in the stopped frame")

        (dap/continue-run!)
        (is (pump! editor #(= :terminated (dap/state)) 20000))
        (is (str/includes? (text-of editor "*Debug Output*") "5")
            "program stdout reaches the output buffer")
        (is (= {:width 1 :marks {2 {:text "●" :face :breakpoint}}}
               (get-in @editor [:bufs path :gutter]))
            "after the session the breakpoint mark remains, the arrow is gone")
        (finally
          (dap/stop!)
          (dap/restore-breakpoints! {})
          (doseq [f (reverse (file-seq dir))] (.delete f)))))
    (println "debugpy not installed (pip install debugpy); skipping live DAP test")))