(ns nesmacs.buffer-lifecycle
  (:require [nesmacs.cmd :as cmd]
            [nesmacs.lsp :as lsp]
            [nesmacs.treesit :as treesit]))

(defn kill-buffer
  "Release a buffer's language tooling and remove it from editor state."
  [s target fallback]
  (when-let [file (:file (cmd/buf s target))]
    (lsp/close-doc! file))
  (treesit/forget! target)
  (-> s
      (cmd/remove-buffer target fallback)
      (cmd/msg (str "Killed " target))))
