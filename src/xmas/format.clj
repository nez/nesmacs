(ns xmas.format
  "External formatters, one argv per language from the language registry.

   Used as the fallback for format-on-save and when the language server has no
   formatting provider. Blocking, like saving itself: formatters are fast and
   the result must be written in the same command."
  (:require [clojure.java.shell :as shell]))

(defn format-text
  "Formatted `text` for language `l`, or nil when there is no formatter, the
   binary is missing, it failed, or it changed nothing. `path` is appended for
   formatters that need the filename (prettier)."
  [l text path dir]
  (when-let [f (:formatter l)]
    (let [argv (cond-> (:argv f) (:path-arg? f) (conj path))
          {:keys [exit out]}
          (try (apply shell/sh (concat argv [:in text :dir dir :out-enc "UTF-8"]))
               (catch Exception _ {:exit 1 :out ""}))]
      (when (and (zero? (long (or exit 1))) (seq out) (not= out text))
        out))))