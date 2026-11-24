(ns vendor-queries
  "Vendor nvim-treesitter query files into resources/queries.

   The editor never downloads queries at runtime. Run this script after
   bumping NVIM_REV, then commit the generated files.

     clojure -M script/vendor_queries.clj

   Normalization keeps only what the JNI binding can evaluate:
   - `; inherits:` is resolved here by concatenating the parent files;
   - `#lua-match?` becomes `#match?` with a small Lua-pattern translation;
   - `#set!` directives are dropped (the binding requires a fixed arity that
     nvim queries do not always follow, and nesmacs does not use metadata)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def NVIM_REV "52773f0d13a0c11d0c3ef33c14ab6ecc1c17148b")
(def BASE (str "https://raw.githubusercontent.com/nvim-treesitter/nvim-treesitter/"
               NVIM_REV "/runtime/queries/"))

;; language -> {:highlights [parent ... child] :folds [...]}
(def languages
  {"python"     {:highlights ["python"] :folds ["python"]}
   "clojure"    {:highlights ["clojure"] :folds ["clojure"]}
   "javascript" {:highlights ["ecma" "jsx" "javascript"]
                 :folds ["ecma" "javascript"]}
   "typescript" {:highlights ["ecma" "typescript"]
                 :folds ["ecma" "typescript"]}
   "tsx"        {:highlights ["ecma" "typescript" "jsx" "tsx"]
                 :folds ["ecma" "typescript" "jsx" "tsx"]}})

(defn fetch [path]
  (let [url (str BASE path)]
    (try (slurp url)
         (catch Exception e
           (throw (ex-info (str "cannot fetch " url) {:url url} e))))))

(defn- lua-append!
  [^StringBuilder sb c]
  (case c
    \u (.append sb "[A-Z]")
    \l (.append sb "[a-z]")
    \d (.append sb "[0-9]")
    \a (.append sb "[A-Za-z]")
    \s (.append sb "\\s")
    \S (.append sb "\\S")
    \w (.append sb "\\w")
    (.append sb (str "\\" c))))

(defn lua-pattern->java
  "Translate the Lua-pattern features used by nvim-treesitter queries.
   Everything else is passed through, including scm escapes such as \\n."
  [^String p]
  (let [sb (StringBuilder.) n (count p)]
    (loop [i 0]
      (if (>= i n)
        (str sb)
        (let [c (.charAt p i)]
          (cond
            (and (= c \%) (< (inc i) n))
            (do (lua-append! sb (.charAt p (inc i))) (recur (+ i 2)))

            (and (= c \\) (< (inc i) n))
            (do (.append sb c) (.append sb (.charAt p (inc i))) (recur (+ i 2)))

            :else (do (.append sb c) (recur (inc i)))))))))

(defn- normalize-lua-match [q]
  (str/replace q
    #"\(#lua-match\?\s+(@[^\s()]+)\s+\"((?:[^\"\\]|\\.)*)\"\)"
    (fn [[_ cap pat]] (str "(#match? " cap " \"" (lua-pattern->java pat) "\")"))))

(defn normalize [q]
  (-> q
      (normalize-lua-match)
      (str/replace #"\(#set!\s+[^()]*\)" "")
      (str/replace #"(?m)^; inherits:.*\n?" "")))

(defn- component [lang kind]
  (normalize (fetch (str lang "/" kind ".scm"))))

(defn- section [lang kind parents]
  (let [body (->> parents
                  (map #(component % kind))
                  (remove str/blank?)
                  (str/join "\n"))
        header (str "; Vendored from nvim-treesitter " NVIM_REV " (Apache-2.0).\n"
                    "; Sources: " (str/join ", " parents) "/" kind ".scm\n"
                    "; Normalized for nesmacs: inherits resolved, #lua-match? -> #match?,\n"
                    "; #set! dropped. Regenerate with script/vendor_queries.clj.\n\n")]
    (str header body "\n")))

(defn -main [& _]
  (let [root (io/file "resources/queries")]
    (.mkdirs root)
    (doseq [[lang {:keys [highlights folds]}] languages]
      (let [dir (io/file root lang)]
        (.mkdirs dir)
        (spit (io/file dir "highlights.scm") (section lang "highlights" highlights))
        (spit (io/file dir "folds.scm") (section lang "folds" folds))
        (println "wrote" (str dir))))
    (spit (io/file root "LICENSE.nvim-treesitter")
          (slurp (str "https://raw.githubusercontent.com/nvim-treesitter/nvim-treesitter/"
                      NVIM_REV "/LICENSE")))
    (spit (io/file root "README.md")
          (str "# Vendored queries\n\n"
               "`highlights.scm` and `folds.scm` come from nvim-treesitter at\n"
               "commit `" NVIM_REV "` (Apache-2.0, see LICENSE.nvim-treesitter).\n"
               "They are generated, not edited: change `script/vendor_queries.clj`\n"
               "and regenerate. `#set!` directives are dropped because the JNI\n"
               "binding does not accept every arity nvim uses; nesmacs does not read\n"
               "query metadata.\n")))
  (println "done"))

(-main)