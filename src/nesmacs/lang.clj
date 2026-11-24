(ns nesmacs.lang
  "Language registry: everything language-specific is data.

   A language entry drives the major mode, comment syntax, formatter,
   language server, debug adapter, Tree-sitter grammar and query set. Adding a
   language is a map entry; no per-language code. Unknown extensions open as
   plain text."
  (:require [clojure.string :as str]))

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
          :launch {:console "internalConsole"}}
    :indent 4}

   :javascript
   {:id "javascript" :mode :javascript-mode :comment-start "// "
    :exts #{"js" "jsx" "mjs" "cjs"}
    :grammar "org.treesitter.TreeSitterJavascript" :queries "javascript"
    :lsp ["typescript-language-server" "--stdio"]
    :formatter {:argv ["prettier" "--stdin-filepath"] :path-arg? true}
    :dap {:transport :tcp
          :argv ["node" "{env:JS_DEBUG_DAP}" "{port}"]
          :launch {:type "pwa-node"}}
    :indent 2}

   :typescript
   {:id "typescript" :mode :typescript-mode :comment-start "// "
    :exts #{"ts" "mts" "cts"}
    :grammar "org.treesitter.TreeSitterTypescript" :queries "typescript"
    :lsp ["typescript-language-server" "--stdio"]
    :formatter {:argv ["prettier" "--stdin-filepath"] :path-arg? true}
    :dap {:transport :tcp
          :argv ["node" "{env:JS_DEBUG_DAP}" "{port}"]
          :launch {:type "pwa-node"}}
    :indent 2}

   :tsx
   {:id "tsx" :mode :tsx-mode :comment-start "// "
    :exts #{"tsx"}
    :grammar "org.treesitter.TreeSitterTsx" :queries "tsx"
    :lsp ["typescript-language-server" "--stdio"]
    :formatter {:argv ["prettier" "--stdin-filepath"] :path-arg? true}
    :dap {:transport :tcp
          :argv ["node" "{env:JS_DEBUG_DAP}" "{port}"]
          :launch {:type "pwa-node"}}
    :indent 2}

   :emacs-lisp
   {:id "emacs-lisp" :mode :emacs-lisp-mode :comment-start ";; "
    :exts #{"el"}
    :indent 2}})

(defn language
  [id]
  (get languages id))

(defn language-for-file
  "Language map for `filename`, or nil when the extension is unknown."
  [^String filename]
  (when (and filename (not (str/blank? filename)))
    (let [dot (.lastIndexOf filename ".")]
      (when (pos? dot)
        (let [ext (str/lower-case (subs filename (inc dot)))]
          (some (fn [[_ l]] (when (contains? (:exts l) ext) l))
                languages))))))

(defn mode-for-file [filename] (:mode (language-for-file filename)))

(defn extensions
  "All known extensions, without the dot."
  []
  (into #{} (mapcat :exts) (vals languages)))