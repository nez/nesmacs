(ns nesmacs.buffer-lifecycle
  (:require [clojure.string :as str]
            [nesmacs.api :as api]
            [nesmacs.cmd :as cmd]
            [nesmacs.files :as files]
            [nesmacs.lsp :as lsp]
            [nesmacs.mini :as mini]
            [nesmacs.treesit :as treesit]))

(defn- remove-buffer
  [s target fallback]
  (let [file (:file (cmd/buf s target))]
    (-> s
        (cmd/remove-buffer target fallback)
        (api/after-command (fn [_]
                             (when file (lsp/close-doc! file))
                             (treesit/forget! target)))
        (cmd/msg (str "Killed " target)))))

(defn kill-buffer
  "Close a buffer, prompting before discarding unsaved edits."
  ([s target fallback] (kill-buffer s target fallback identity))
  ([s target fallback on-close]
   (let [b (cmd/buf s target)
         close #(on-close (remove-buffer % target fallback))]
     (if (:modified b)
       (mini/start s (str "Close " (:name b) "? (" (when (:file b) "s save, ") "d discard, c cancel) ")
         (fn [s answer]
           (let [current (cmd/buf s target)]
             (case (str/lower-case (str/trim answer))
               "s" (if (:file current)
                     (-> s (files/save-buffer-named target)
                         (api/after-command
                           (fn [publish!]
                             (publish! #(if (and (cmd/buf % target) (not (:modified (cmd/buf % target))))
                                          (close %) %)))))
                     (kill-buffer s target fallback on-close))
               "d" (if (identical? (:text b) (:text current))
                     (close s)
                     (kill-buffer s target fallback on-close))
               "c" (cmd/msg s "Close canceled")
               (kill-buffer s target fallback on-close)))))
       (if b (close s) (cmd/msg s (str "No buffer: " target)))))))
