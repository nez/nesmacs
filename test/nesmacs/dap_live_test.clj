(ns ^:integration nesmacs.dap-live-test
  "End-to-end checks against the real DAP adapters. Excluded from `make test`
   (they need Python with debugpy and Node with js-debug); run `make dap`.

   Set NESMACS_DEBUGPY to the Python executable that has debugpy installed, and
   JS_DEBUG_DAP to vscode-js-debug's dapDebugServer.js."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [nesmacs.buf :as buf]
            [nesmacs.dap :as dap]
            [nesmacs.lang :as lang]
            [nesmacs.testutil :as tu]))

(defn- python-with-debugpy []
  (let [py (or (System/getenv "NESMACS_DEBUGPY") "python3")]
    (when (zero? (:exit (shell/sh py "-c" "import debugpy")))
      py)))

(defn- js-debug-server []
  (when-let [p (System/getenv "JS_DEBUG_DAP")]
    (when (.isFile (java.io.File. ^String p)) p)))

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
    (let [dir (tu/temp-dir "nesmacs-dap-live")
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
          (tu/delete-tree dir))))
    (println "debugpy not installed (pip install debugpy); skipping live DAP test")))

(deftest js-debug-session-hits-a-breakpoint-and-reports-state
  (if-let [server (js-debug-server)]
    (let [dir (tu/temp-dir "nesmacs-dap-js")
          prog (java.io.File. ^java.io.File dir "prog.js")
          _ (spit prog (str "function add(a, b) {\n"
                            "  const total = a + b;\n"
                            "  return total;\n"
                            "}\n\n"
                            "console.log(add(2, 3));\n"))
          path (.getPath prog)
          editor (atom {:bufs {path (buf/make path (slurp prog) path)}
                        :msg nil :rows 24 :cols 80})
          stop (atom nil)
          language (lang/language :javascript)
          ;; Assemble argv here so the test does not depend on the environment
          ;; expansion path; `{port}` still exercises the TCP transport.
          cfg (assoc-in (:dap language) [:argv] ["node" server "{port}"])]
      (try
        (dap/restore-breakpoints! {})
        (dap/set-breakpoints! path [2])          ; 0-based: `return total`
        (is (re-find #"Starting javascript debugger"
                     (dap/start! {:editor editor :root (.getPath ^java.io.File dir)
                                  :file path :language language :dap cfg
                                  :on-stop (fn [s frame] (reset! stop frame) s)})))
        (is (pump! editor #(= :stopped (dap/state)) 30000) "the breakpoint is hit")
        (is (pump! editor #(some? (get-in @editor [:bufs "*Debug*"])) 10000))
        (is (pump! editor #(some? (get-in @editor [:bufs "*Debug Locals*"])) 10000))

        (is (= {:file path :line 2 :id (:id @stop)}
               (select-keys @stop [:file :line :id]))
            "on-stop points at the source line")
        (is (str/includes? (text-of editor "*Debug*") "add"))
        (let [locals (text-of editor "*Debug Locals*")]
          (is (str/includes? locals "total"))
          (is (re-find #"= 5\b" (or locals ""))))
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
          (tu/delete-tree dir))))
    (println "JS_DEBUG_DAP not set; skipping js-debug live DAP test")))
