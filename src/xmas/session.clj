(ns xmas.session
  "Session persistence: save/restore buffers, windows, and editor state.
   Undo history is explicitly NOT persisted (per roadmap)."
  (:require [xmas.buf :as buf]
            [xmas.persist :as persist]
            [xmas.window :as win]))

(def ^:private session-file "session")
(def ^:private session-version 1)

(defn- buffer->data
  "Serialize a buffer to EDN-safe data. Excludes undo/redo and runtime state."
  [b]
  (let [text (str (:text b))]
    {:name (:name b)
     :text text
     :point (:point b)
     :mark (:mark b)
     :file (:file b)
     :modified (:modified b)
     :mode (:mode b)
     :version (:version b)
     :edit-count (:edit-count b 0)
     :line-states (:line-states b [])
     :props (:props b [])
     :large-file (:large-file b)}))

(defn- data->buffer
  "Deserialize a buffer from session data."
  [d]
  (-> (buf/make (:name d) (:text d) (:file d))
      (assoc :point (:point d)
             :mark (:mark d)
             :modified (:modified d)
             :mode (:mode d)
             :version (:version d)
             :edit-count (:edit-count d)
             :line-states (:line-states d)
             :props (:props d)
             :large-file (:large-file d))))

(defn- window-tree->data
  "Serialize window tree, replacing buffer refs with names."
  [tree]
  (letfn [(walk [node]
            (if (win/leaf? node)
              {:type :leaf
               :buffer (:buffer node)
               :scroll (:scroll node 0)
               :hscroll (:hscroll node 0)
               :point (:point node)
               :mark (:mark node)}
              {:type :split
               :dir (:dir node)
               :ratio (:ratio node 0.5)
               :a (walk (:a node))
               :b (walk (:b node))}))]
    (walk tree)))

;; --- Periodic save ---

(def ^:private save-counter (atom 0))
(def ^:private save-interval 50) ; ~5 seconds (50 * 100ms ticks)
;; shortcut: up to ~5 s of edits can be lost on kill -9; auto-save covers
;; file buffers sooner, lower this only with cheap text hashing.
(def ^:private last-sig (atom nil))

(defn signature
  "Cheap change signature: buffer versions/points and layout, not buffer text.
   Lets the periodic save skip idle ticks without re-serializing 10 MB buffers."
  [s]
  (hash [(mapv (fn [[name b]]
                 [name (:version b) (:point b) (:modified b)])
               (:bufs s))
         (:windows s) (:cur-window s)
         (:recentf s) (:bookmarks s) (:mini-history s)]))

(defn save-session!
  "Save current editor state to ~/.xmas/session.edn.
   Called on clean exit and periodically."
  [state]
  (let [bufs (:bufs state)
        buf-data (update-vals bufs buffer->data)
        session {:version session-version
                 :timestamp (System/currentTimeMillis)
                 :bufs buf-data
                 :windows (window-tree->data (:windows state))
                 :cur-window (:cur-window state)
                 :recentf (:recentf state [])
                 :bookmarks (:bookmarks state {})
                 :mini-history (:mini-history state [])}]
    (persist/save-quiet! session-file session)
    session))

(defn save-tick!
  "Editor-loop step: save the session every ~5s, but only when it changed."
  [editor-atom]
  (when (>= (swap! save-counter inc) save-interval)
    (reset! save-counter 0)
    (let [s @editor-atom
          sig (signature s)]
      (when (not= sig @last-sig)
        (reset! last-sig sig)
        (save-session! s)))))

(defn- sanitize-session
  "Validate and clean loaded session data."
  [data]
  (when (and (map? data)
             (= session-version (:version data)))
    (let [bufs (:bufs data)
          windows (:windows data)
          cur-window (:cur-window data)
          recentf (if (sequential? (:recentf data))
                    (vec (filter string? (:recentf data)))
                    [])
          bookmarks (if (map? (:bookmarks data))
                      (into {}
                            (filter (fn [[_ v]]
                                      (and (string? (:buf v))
                                           (nat-int? (:pos v)))))
                            (:bookmarks data))
                      {})
          mini-history (if (sequential? (:mini-history data))
                         (filterv string? (:mini-history data))
                         [])]
      {:bufs bufs
       :windows windows
       :cur-window cur-window
       :recentf recentf
       :bookmarks bookmarks
       :mini-history mini-history})))

(defn load-session!
  "Load session from ~/.xmas/session.edn. Returns nil if no valid session."
  []
  (when-let [data (persist/load! session-file)]
    (sanitize-session data)))

(defn restore-session
  "Apply loaded session data to a fresh editor state.
   Returns [state restored?] where restored? is true if session was applied."
  [state session-data]
  (when session-data
    (let [bufs (:bufs session-data)
          windows (:windows session-data)
          cur-window (:cur-window session-data)
          recentf (:recentf session-data)
          bookmarks (:bookmarks session-data)
          mini-history (:mini-history session-data)

          ;; Rebuild buffers from saved data
          rebuilt-bufs (update-vals bufs data->buffer)

          ;; Filter out buffers that no longer exist on disk (optional)
          valid-bufs (into {}
                          (filter (fn [[_ b]]
                                    (or (nil? (:file b))
                                        (.exists (java.io.File. (:file b)))))
                                rebuilt-bufs))

          ;; Ensure cur-window points to an existing buffer
          leaf-paths (win/leaves windows)
          valid-cur-window (if (and cur-window
                                    (get-in windows cur-window))
                             cur-window
                             (first leaf-paths))

          ;; Rebuild window tree with actual buffer objects
          rebuilt-windows (letfn [(walk [node]
                                    (if (win/leaf? node)
                                      (let [buf-name (:buffer node)]
                                        (if (contains? valid-bufs buf-name)
                                          (assoc (win/leaf buf-name
                                                           (:scroll node 0)
                                                           (:hscroll node 0))
                                                 :point (:point node)
                                                 :mark (:mark node))
                                          (win/leaf "*scratch*")))
                                      (assoc (win/split-node (:dir node)
                                                             (walk (:a node))
                                                             (walk (:b node)))
                                             :ratio (:ratio node 0.5))))]
                            (walk windows))

          new-state (-> state
                        (assoc :bufs valid-bufs)
                        (assoc :windows rebuilt-windows)
                        (assoc :cur-window valid-cur-window)
                        (assoc :recentf recentf)
                        (assoc :bookmarks bookmarks)
                        (assoc :mini-history mini-history)
                        ;; Point current window to its buffer
                        (update-in (conj valid-cur-window :buffer)
                                   (fn [_] (get-in rebuilt-windows valid-cur-window :buffer))))]
      [new-state true])))