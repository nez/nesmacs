(ns nesmacs.lang
  "Language registry: everything language-specific is data.

   A language entry drives the major mode, comment syntax, formatter,
   language server, debug adapter, Tree-sitter grammar and query set. Adding a
   language is a map entry; no per-language code. Unknown extensions open as
   plain text."
  (:require [clojure.string :as str]))

(def ^:private javascript-tools
  {:comment-start "// "
   :lsp ["typescript-language-server" "--stdio"]
   :formatter {:argv ["prettier" "--stdin-filepath"] :path-arg? true}
   :dap {:transport :tcp
         :argv ["node" "{env:JS_DEBUG_DAP}" "{port}"]
         :launch {:type "pwa-node"}
         :attach {:type "pwa-node"}}
   :indent 2})

(def languages
  {:clojure
   {:id "clojure" :mode :clojure-mode :comment-start ";; "
    :exts #{"clj" "cljc" "cljs" "edn"}
    :grammar "org.treesitter.TreeSitterClojure" :queries "clojure"
    :lsp ["clojure-lsp"]
    :formatter {:argv ["cljfmt" "fix" "-"]}
    :indent 2}

   :python
   {:id "python" :mode :python-mode :comment-start "# "
    :exts #{"py" "pyi"}
    :grammar "org.treesitter.TreeSitterPython" :queries "python"
    :lsp ["pyright-langserver" "--stdio"]
    :formatter {:argv ["ruff" "format" "-"]}
    :dap {:transport :stdio
          :argv ["python3" "-m" "debugpy.adapter"]
          :launch {:console "internalConsole"}
          :attach {:justMyCode true}}
    :indent 4}

   :javascript
   (merge javascript-tools
          {:id "javascript" :mode :javascript-mode :exts #{"js" "jsx" "mjs" "cjs"}
           :grammar "org.treesitter.TreeSitterJavascript" :queries "javascript"})

   :typescript
   (merge javascript-tools
          {:id "typescript" :mode :typescript-mode :exts #{"ts" "mts" "cts"}
           :grammar "org.treesitter.TreeSitterTypescript" :queries "typescript"})

   :tsx
   (merge javascript-tools
          {:id "tsx" :mode :tsx-mode :exts #{"tsx"}
           :grammar "org.treesitter.TreeSitterTsx" :queries "tsx"})

   :emacs-lisp
   {:id "emacs-lisp" :mode :emacs-lisp-mode :comment-start ";; "
    :exts #{"el"}
    :indent 2}})

(defonce ^:private extensions-by-language (atom {}))

(defn register! [id spec]
  (swap! extensions-by-language assoc id (assoc spec :id (name id))))

(defn- registry [] (merge languages @extensions-by-language))

(defn language
  [id]
  (get (registry) id))

(defn language-for-file
  "Language map for `filename`, or nil when the extension is unknown."
  [^String filename]
  (when (and filename (not (str/blank? filename)))
    (let [dot (.lastIndexOf filename ".")]
      (when (pos? dot)
        (let [ext (str/lower-case (subs filename (inc dot)))]
          (some (fn [[_ l]] (when (contains? (:exts l) ext) l))
                (registry)))))))

(defn mode-for-file [filename] (:mode (language-for-file filename)))

(defn with-file-language [b]
  (if-let [l (language-for-file (:file b))]
    (assoc b :language l :comment-start (:comment-start l))
    b))

(defn extensions
  "All known extensions, without the dot."
  []
  (into #{} (mapcat :exts) (vals (registry))))
