(ns ^:integration nesmacs.grammar-live-test
  "Clean-install check for every pinned Tree-sitter grammar and vendored query
   set. Run with `make grammars`, which resolves the dependencies into an empty
   local Maven repo (`.cpcache/m2-check`), so a warm cache cannot hide a missing
   artifact or native library. A grammar that fails to load makes `spans` nil
   and the test fails instead of silently falling back to the tokenizer."
  (:require [clojure.test :refer [deftest is testing]]
            [nesmacs.lang :as lang]
            [nesmacs.treesit :as treesit]))

(def ^:private samples
  {:clojure "(defn f [x]\n  (inc x))\n"
   :python "def f(x):\n    return x + 1\n"
   :javascript "function f(x) {\n  return x + 1;\n}\n"
   :typescript "function f(x: number): number {\n  return x + 1;\n}\n"
   :tsx "const f = (x: number) => (\n  <div>{x}</div>\n);\n"})

(deftest every-pinned-grammar-loads-and-queries
  ;; The first parse in a JVM pays for JNI linking and query compilation, which
  ;; is far above the editor's per-keystroke budget. This test is about artifacts
  ;; and vendored queries, not latency (the budget has its own unit test), so a
  ;; cold parse must not trip the tokenizer fallback.
  (binding [treesit/*slow-parse-ms* 60000]
    (doseq [[id text] samples]
      (let [language (lang/language id)
            key (str "grammar-check-" (name id))]
        (try
          (is (seq (treesit/spans key language text 1))
              (str id " loads " (:grammar language) " and its highlights query"))
          (is (seq (treesit/folds key language text 1)) (str id " folds query loads"))
          (finally
            (treesit/forget! key)))))))

(deftest unknown-grammar-still-falls-back
  (testing "the fallback path is not broken by the grammar check"
    (is (nil? (treesit/spans "grammar-check-missing"
                             {:grammar "does.not.Exist" :id "nope"} "x" 1)))))