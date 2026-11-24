(ns nesmacs.buflist
  "*Buffer List* buffer built on listbuf: tabular view of all open buffers
   with keys to switch, kill, or refresh."
  (:require [nesmacs.cmd :as cmd]
            [nesmacs.listbuf :as listbuf]
            [nesmacs.lsp :as lsp]
            [nesmacs.treesit :as treesit]
            [nesmacs.window :as win]))

(def ^:private buflist-name "*Buffer List*")

(defn- format-entry [b]
  (format "  %s %-20s %s"
          (if (:modified b) "*" " ")
          (or (:name b) "")
          (or (:file b) "")))

(defn- visible-bufs
  "Buffer maps for every visible buffer, each tagged with :key — its entry in
   :bufs. Buffer names are only display labels (a file buffer's name is its
   basename), so switching/killing must use the key, not the name."
  [bufs]
  (->> bufs
       (keep (fn [[k b]]
               (when-not (or (= k buflist-name) (.startsWith ^String k " "))
                 (assoc b :key k))))
       (sort-by :name)
       vec))

(defn- spec [bufs]
  {:name         buflist-name
   :mode         :buflist-mode
   :header       "  M Name                 File"
   :entries      (visible-bufs bufs)
   :format-entry format-entry})

(defn open [s]
  (-> s (listbuf/refresh buflist-name (spec (:bufs s)))
        (cmd/set-cur-buffer buflist-name)))

;; --- Commands ---

(defn switch
  "Switch to the buffer on the current line."
  [s]
  (if-let [b (listbuf/entry-at-point s)]
    (cmd/set-cur-buffer s (:key b))
    s))

(defn kill
  "Kill the buffer on the current line, then refresh the list."
  [s]
  (if-let [b (listbuf/entry-at-point s)]
    (let [target (:key b)
          ;; Same cleanup as ed/kill-buffer: a killed file buffer must close
          ;; its LSP document and drop the native parse state.
          _ (when-let [f (:file (get-in s [:bufs target]))] (lsp/close-doc! f))
          _ (treesit/forget! target)
          s' (-> s
                 (update :bufs dissoc target)
                 ;; windows showing the killed buffer fall back to the buflist
                 (update :windows win/replace-buffer target buflist-name))
          s' (cond-> s'
               (= target (:buf s')) (assoc :buf buflist-name))]
      (-> s' (listbuf/refresh buflist-name (spec (:bufs s')))
             (cmd/msg (str "Killed " target))))
    s))

(defn revert
  "Re-render the buffer list from current :bufs state."
  [s]
  (listbuf/refresh s buflist-name (spec (:bufs s))))
