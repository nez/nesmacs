(ns ^:integration xmas.lsp-live-test
  "End-to-end check against the real clojure-lsp binary. Excluded from
   `make test` (it needs the server and its indexing time); run `make lsp`."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [xmas.buf :as buf]
            [xmas.lsp :as lsp]
            [xmas.proc :as proc]
            [xmas.testutil :as tu]))

(defn- lsp-on-path? []
  (some (fn [dir]
          (let [f (java.io.File. ^String (str dir java.io.File/separator "clojure-lsp"))]
            (and (.isFile f) (.canExecute f))))
        (str/split (or (System/getenv "PATH") "")
                   (re-pattern java.io.File/pathSeparator))))

(defn- pump!
  "Tick the LSP client until `pred` holds or the deadline passes."
  [editor pred ms]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop []
      (lsp/tick! editor)
      (cond (pred) true
            (> (System/currentTimeMillis) end) false
            :else (do (Thread/sleep 100) (recur))))))

(defn- items-of
  "Servers may answer completion with a CompletionList map or a raw vector."
  [r]
  (cond (map? r) (:items r)
        (vector? r) r
        :else nil))

(defn- ask-until
  "Send a request repeatedly until `ok?` accepts the answer or time runs out.
   A freshly started server answers null/empty until analysis completes, so a
   single request is not a fair test."
  [editor send! ok? ms]
  (let [answer (atom ::none)
        end    (+ (System/currentTimeMillis) ms)]
    (loop []
      (reset! answer ::none)
      (send! (fn [r] (reset! answer r)))
      (pump! editor #(not= ::none @answer) 10000)
      (cond (ok? @answer) @answer
            (> (System/currentTimeMillis) end) (do (println "LSP answer before timeout:" (pr-str @answer)) false)
            :else (do (Thread/sleep 500) (recur))))))

(deftest ^:integration clojure-lsp-end-to-end
  (if-not (lsp-on-path?)
    (println "clojure-lsp not on PATH; skipping live LSP test")
    (let [dir  (tu/temp-dir "xmas-lsp-live")
          src  (java.io.File. dir "demo.clj")
          text (str "(ns demo)\n\n"
                    "(defn add [a b]\n  (+ a b))\n\n"
                    "(defn broken []\n  (undefined-thing 1))\n\n"
                    "(defn uses-add []\n  (add 1 2))\n")
          _    (spit src text)
          _    (spit (java.io.File. dir "deps.edn") "{}\n")
          editor (atom {:bufs {"demo.clj"
                               (assoc (buf/make "demo.clj" text (.getPath src))
                                      :journal? false)}})
          path (.getPath src)
          root (.getPath dir)
          ready (atom false)
          bufs #(:bufs @editor)]
      (try
        (lsp/ensure! root ["clojure-lsp"] root (fn [_] (reset! ready true)))
        (is (pump! editor #(deref ready) 60000) "server initialized")

        (is (pump! editor #(seq (:props (get-in (bufs) ["demo.clj"]))) 60000)
            "diagnostics arrived")
        (let [p (first (:props (get-in (bufs) ["demo.clj"])))]
          (is (= :diag-error (:face p)))
          (is (= "undefined-thing"
                 (subs (str (:text (get-in (bufs) ["demo.clj"])))
                       (:from p) (:to p)))))

        (let [hovered (atom nil)]
          (lsp/hover root path 2 7 (fn [r] (reset! hovered r)))
          (is (pump! editor #(some? @hovered) 30000) "hover answered")
          (is (some? (:contents @hovered))))

        (let [items (ask-until editor
                               #(lsp/completion root path 9 6 %)
                               #(some (fn [i] (= "add" (:label i))) (items-of %))
                               60000)]
          (is (some #(= "add" (:label %)) (items-of items)) "completion offers add"))

        (let [found (ask-until editor
                               #(lsp/definition root path 9 4 %)
                               #(when-let [loc (if (vector? %1) (first %1) %1)]
                                  (= 2 (get-in loc [:range :start :line])))
                               60000)]
          (is (some? found) "definition points at add")
          (when found
            (let [loc (if (vector? found) (first found) found)]
              (is (= path (lsp/uri->path (or (:uri loc) (:targetUri loc))))))))

        ;; Crash recovery: kill the server and ask again; ensure! must start a
        ;; fresh one and serve requests.
        (proc/kill-all!)
        (is (pump! editor #(not (lsp/server-exists? root)) 30000) "dead server dropped")
        (reset! ready false)
        (lsp/ensure! root ["clojure-lsp"] root (fn [_] (reset! ready true)))
        (is (pump! editor #(deref ready) 60000) "server restarted")
        (finally
          (lsp/shutdown-all!)
          (tu/delete-tree dir))))))