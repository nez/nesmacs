(ns ^:integration nesmacs.lsp-live-test
  "End-to-end checks against the real language servers. One project per
   language, so diagnostics, hover, completion and definition run through the
   same client the editor uses. Excluded from `make test` (servers and their
   indexing time); run `make lsp`.

   A server missing from PATH is a skip, not a failure. Set NESMACS_CLOJURE_LSP,
   NESMACS_PYRIGHT or NESMACS_TSSERVER to an executable outside PATH."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [nesmacs.buf :as buf]
            [nesmacs.lang :as lang]
            [nesmacs.lsp :as lsp]
            [nesmacs.proc :as proc]
            [nesmacs.testutil :as tu]))

(defn- server-argv
  "The language's configured server, with the executable overridable by env
   (npm installs it outside PATH on many machines)."
  [language env]
  (let [[exe & args] (:lsp language)]
    (into [(or (System/getenv env) exe)] args)))

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

(defn- typescript-near
  "A `typescript` package near `server` (project-local or global npm layout).
   typescript-language-server refuses to start without one in the workspace."
  [^String server]
  (let [start (.getParentFile (.getCanonicalFile (java.io.File. server)))]
    (loop [d start]
      (when d
        (or (some (fn [f] (when (.isDirectory ^java.io.File f) f))
                  [(java.io.File. ^java.io.File d "node_modules/typescript")
                   (java.io.File. ^java.io.File d "typescript")])
            (recur (.getParentFile ^java.io.File d)))))))

(defn- link-typescript!
  "Point the fixture's node_modules at a TypeScript install; ts-lsp resolves it
   from the workspace root."
  [dir argv]
  (when-let [ts (or (some-> (System/getenv "NESMACS_TYPESCRIPT") java.io.File.)
                    (typescript-near (first argv)))]
    (let [nm (java.io.File. ^java.io.File dir "node_modules")]
      (.mkdirs nm)
      (java.nio.file.Files/createSymbolicLink
       (.toPath (java.io.File. nm "typescript"))
       (.toPath ^java.io.File ts)
       (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- source-of [text from to] (subs text (long from) (long to)))

(defn- diag-at
  "The diagnostic prop whose source text is exactly `word`."
  [bufs path text word]
  (some (fn [p] (when (= word (source-of text (:from p) (:to p))) p))
        (:props (get (bufs) path))))

(defn- check-server
  "Start `language` on a temp project and assert diagnostics, hover, completion
   and definition. `spec` keys:
     :id         language id
     :env        env var overriding the server executable
     :file       source file name
     :extra      {file content} project files (config, deps.edn, ...)
     :text       source text
     :diag       word the server must report as an error
     :hover      [line char]
     :complete   [line char label]
     :definition [line char expected-0-based-line]
     :restart?   kill the server and require a fresh one to answer"
  [{:keys [id env file extra text diag hover complete definition restart?]}]
  (let [language (lang/language id)
        argv     (server-argv language env)]
    (if-not (lsp/on-path? argv)
      (println (first argv) "not on PATH; skipping live LSP test for" id)
      (let [dir    (tu/temp-dir (str "nesmacs-lsp-" (name id)))
            path   (.getPath (java.io.File. ^String dir file))
            _      (spit path text)
            _      (doseq [[n content] extra]
                     (spit (java.io.File. ^String dir n) content))
            linked? (or (not (contains? #{:typescript :tsx} id))
                        (link-typescript! dir argv))]
        (if-not linked?
          (do (println "no typescript package found for" (first argv)
                       "(set NESMACS_TYPESCRIPT); skipping live LSP test for" id)
              (tu/delete-tree dir))
          (let [editor (atom {:bufs {path (assoc (buf/make path text path) :journal? false)}
                              :msg nil :rows 24 :cols 80})
                root   (.getPath ^java.io.File dir)
                ready  (atom false)
                bufs   #(:bufs @editor)]
            (try
              (lsp/ensure! root argv root (:id language) (fn [_] (reset! ready true)))
              (is (pump! editor #(deref ready) 60000) (str id " server initialized"))

              (testing "diagnostics become props"
                (is (pump! editor #(some? (diag-at bufs path text diag)) 60000)
                    (str id " reports " diag))
                (when-let [p (diag-at bufs path text diag)]
                  (is (= :diag-error (:face p)))))

              (testing "hover"
                (let [[line char] hover
                      answer (atom ::none)]
                  (lsp/hover root path line char (fn [r] (reset! answer r)))
                  (is (pump! editor #(not= ::none @answer) 30000) "hover answered")
                  (is (some? (:contents @answer)))))

              (testing "completion"
                (let [[line char label] complete
                      found (ask-until editor #(lsp/completion root path line char %)
                                       #(some (fn [i] (= label (:label i))) (items-of %))
                                       60000)]
                  (is (some #(= label (:label %)) (items-of found))
                      (str id " completion offers " label))))

              (testing "definition"
                (let [[line char expected] definition
                      found (ask-until editor #(lsp/definition root path line char %)
                                       #(when-let [loc (if (vector? %1) (first %1) %1)]
                                          (= expected (get-in loc [:range :start :line])))
                                       60000)]
                  (is (some? found) (str id " definition points at line " expected))
                  (when found
                    (let [loc (if (vector? found) (first found) found)]
                      (is (= path (lsp/uri->path (or (:uri loc) (:targetUri loc)))))))))

              (when restart?
                (testing "restart after the server dies"
                  (proc/kill-all!)
                  (is (pump! editor #(not (lsp/server-exists? root)) 30000) "dead server dropped")
                  (reset! ready false)
                  (lsp/ensure! root argv root (:id language) (fn [_] (reset! ready true)))
                  (is (pump! editor #(deref ready) 60000) "server restarted")
                  (let [[line char label] complete]
                    (is (some #(= label (:label %))
                              (items-of (ask-until editor #(lsp/completion root path line char %)
                                                   #(some (fn [i] (= label (:label i))) (items-of %))
                                                   60000)))
                        "the restarted server answers requests"))))
              (finally
                (lsp/shutdown-all!)
                (proc/kill-all!)
                (tu/delete-tree dir)))))))))

(def ^:private clojure-text
  (str "(ns demo)\n\n"
       "(defn add [a b]\n  (+ a b))\n\n"
       "(defn broken []\n  (undefined-thing 1))\n\n"
       "(defn uses-add []\n  (add 1 2))\n"))

(def ^:private python-text
  (str "def add(a, b):\n"
       "    return a + b\n"
       "\n"
       "add(1, 2)\n"
       "nope\n"
       "ad\n"))

(def ^:private typescript-text
  (str "export function add(a: number, b: number): number {\n"
       "  return a + b;\n"
       "}\n"
       "\n"
       "add(1, 2);\n"
       "nope;\n"
       "ad\n"))

(deftest clojure-lsp-end-to-end
  (check-server
   {:id :clojure :env "NESMACS_CLOJURE_LSP" :file "demo.clj"
    :extra {"deps.edn" "{}\n"}
    :text clojure-text
    :diag "undefined-thing"
    :hover [2 7] :complete [9 6 "add"] :definition [9 4 2]
    :restart? true}))

(deftest pyright-end-to-end
  (check-server
   {:id :python :env "NESMACS_PYRIGHT" :file "demo.py"
    :extra {"pyrightconfig.json" "{\"typeCheckingMode\": \"basic\"}\n"}
    :text python-text
    :diag "nope"
    :hover [0 5] :complete [5 2 "add"] :definition [3 1 0]}))

(deftest typescript-language-server-end-to-end
  (check-server
   {:id :typescript :env "NESMACS_TSSERVER" :file "demo.ts"
    :extra {"tsconfig.json" "{\"compilerOptions\": {\"strict\": true, \"noEmit\": true}}\n"}
    :text typescript-text
    :diag "nope"
    :hover [0 17] :complete [6 2 "add"] :definition [4 1 0]}))