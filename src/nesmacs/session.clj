(ns nesmacs.session
  "Session persistence: save/restore buffers, windows, and editor state.
   Undo history is explicitly NOT persisted (per roadmap)."
  (:require [nesmacs.buf :as buf]
            [nesmacs.dap :as dap]
            [nesmacs.gap :as gap]
            [nesmacs.lang :as lang]
            [nesmacs.log :as log]
            [nesmacs.persist :as persist]
            [nesmacs.window :as win]))

(def ^:private session-file "session")
(def ^:private journal-file "journal")
(def ^:private session-version 1)

(defn- buffer->data
  "Serialize a buffer to EDN-safe data. Excludes undo/redo and runtime state.
   An unmodified file-backed buffer stores no text: the file is the source, and
   re-serializing a 10 MB buffer every few seconds is pure waste. Modified
   buffers keep their text so `kill -9` still loses nothing.

   Fontification spans are derived, so only the per-line lexer states are
   persisted (as [nil state]); when they are saved alongside the text, a
   restart resumes from them instead of re-tokenizing from line 0. Buffers
   whose text is not persisted (unmodified files) save no states either, so
   the session file stays small and a changed file is reconciled from disk."
  [b]
  (let [^String file (:file b)
        on-disk? (and file (not (:modified b))
                      (.canRead (java.io.File. file)))
        term (:terminal b)]
    (cond->
      {:name (:name b)
       :text (when-not on-disk? (str (:text b)))
       :point (:point b)
       :mark (:mark b)
       :file file
       :line-ending (:line-ending b)
       :modified (:modified b)
       :mode (:mode b)
       :version (:version b)
       :edit-count (:edit-count b 0)
       :font-lock (when-not on-disk?
                    (mapv (fn [entry] [nil (second entry)]) (:font-lock b)))
       :props (:props b [])
       :collapsed (:collapsed b)
       :large-file (:large-file b)}
      ;; The screen is a cache: reattaching captures it again. Persist only the
      ;; session identity and geometry, never the (large, changing) snapshot.
      term (assoc :text nil :font-lock [] :props []
                  :terminal (select-keys term [:id :argv :dir :cols :rows])))))

(defn- read-buffer-file
  "Re-read a buffer's file for restore. nil when it can no longer be read, so
   the caller drops the buffer instead of presenting empty text for a real file."
  [^String path]
  (try (persist/read-file path)
       (catch Exception _ nil)))

(defn- sanitize-terminal [t]
  (when (and (map? t)
             (string? (:id t))
             (sequential? (:argv t))
             (every? string? (:argv t)))
    {:id (:id t)
     :argv (vec (:argv t))
     :dir (when (string? (:dir t)) (:dir t))
     :cols (max 1 (long (if (int? (:cols t)) (:cols t) 80)))
     :rows (max 1 (long (if (int? (:rows t)) (:rows t) 24)))}))

(defn- safe-int [x] (if (integer? x) x 0))

(defn- data->buffer
  "Deserialize a buffer from session data. Returns nil if its file-backed text
   is missing from the session and can no longer be read from disk."
  [d]
  ;; A hand-edited or truncated session file must not crash startup: coerce
  ;; every field a later `long`/`File` call would choke on.
  (let [file (when (string? (:file d)) (:file d))
        saved (when (string? (:text d)) (:text d))
        term (sanitize-terminal (:terminal d))
        disk (when (and file (or (nil? saved) (nil? (:line-ending d))))
               (read-buffer-file file))
        text (or saved (:text disk))]
    (when (or text (nil? file))
      ;; Text reloaded from disk may have changed since the session was saved,
      ;; so cached fontification and props no longer line up. Fontification is
      ;; derived: it is reused only when the text came from the session, and
      ;; starts empty when the text is reloaded from disk. Props ride along
      ;; only with session text, since their offsets would otherwise be stale.
      (let [b (-> (buf/make (:name d) (or text "") file (or (:line-ending disk) (:line-ending d)))
                  (assoc :modified (:modified d)
                         :mode (:mode d)
                         :version (safe-int (:version d))
                         :edit-count (safe-int (:edit-count d))
                         :journal-from-disk? (and (some? file) (nil? saved))
                         :font-lock (if (and saved (coll? (:font-lock d)))
                                      (vec (:font-lock d)) [])
                         :props (if (and saved (coll? (:props d)))
                                  (vec (:props d)) [])
                         :large-file (:large-file d)))]
        ;; A terminal's text is a capture, not a document: start empty and let
        ;; the backend reattach and paint it. `:create?` is true because the
        ;; tmux session may be gone (reboot, tmux server killed): `new-session
        ;; -A` reuses it when it exists and otherwise gives the buffer a shell
        ;; again instead of leaving a dead empty pane.
        (if term
          (-> b
              (assoc :text (gap/of "") :point 0 :mark nil :props []
                     :font-lock [] :font-lock-window nil :large-file false
                     :read-only true :journal? false :mode :terminal-mode
                     :terminal (assoc term :status :detached :create? true
                                      :in [] :dirty? true :last-capture 0)))
          ;; A file reloaded from disk may have shrunk, so the saved offsets can
          ;; point past the end.
          (cond-> (-> b
                      (assoc :point (buf/clamp-pos b (safe-int (:point d))))
                      lang/with-file-language)
            (integer? (:mark d)) (assoc :mark (buf/clamp-pos b (:mark d)))
            (coll? (:collapsed d))
            (assoc :collapsed
                   (into #{} (filter #(and (integer? %)
                                           (< -1 (long %) (gap/line-count (:text b)))))
                         (:collapsed d)))))))))

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
(def ^:private last-snapshot (atom nil))

;; Snapshots run off the editor thread. Sequence numbers make a later write
;; win even when two saves overlap: an older snapshot never clobbers a newer
;; one, and the final save on exit always beats an in-flight background one.
;; `write-lock` serializes the (slow) session writes; `journal-lock` only
;; guards the small journal file, so a keystroke never waits for a 10 MB write.
(def ^:private write-lock (Object.))
(def ^:private journal-lock (Object.))
(def ^:private write-seq (atom 0))
(def ^:private written-seq (atom 0))
(def ^:private save-pending? (atom false))

(defn signature
  "Cheap change signature: buffer versions/points and layout, not buffer text.
   Lets the periodic save skip idle ticks without re-serializing 10 MB buffers."
  [s]
  (hash [(mapv (fn [[name b]]
                 [name (:version b) (:point b) (:modified b)])
               (:bufs s))
         (:windows s) (:cur-window s)
         (:recentf s) (:bookmarks s) (:mini-history s) (:theme s :default)
         (dap/snapshot-breakpoints)]))

(defn- structure-signature
  "The part of the session that must reach disk soon: which buffers exist and
   how the windows show them. An edit can be rebuilt from the journal, but a
   buffer opened after the last snapshot would otherwise vanish."
  [s]
  (hash [(mapv (fn [[name b]] [name (:file b)]) (:bufs s))
         (:windows s) (:cur-window s)]))

(defn- snapshot-signatures [state]
  {:signature (signature state) :structure (structure-signature state)})

(defn- session-data [state]
  (let [bufs (:bufs state)]
    {:version session-version
     :timestamp (System/currentTimeMillis)
     :bufs (update-vals bufs buffer->data)
     :windows (window-tree->data (:windows state))
     :cur-window (:cur-window state)
     :recentf (:recentf state [])
     :bookmarks (:bookmarks state {})
     :mini-history (:mini-history state [])
     :theme (:theme state :default)
     :breakpoints (dap/snapshot-breakpoints)}))

(defn- trim-journal!
  "Drop journal records already covered by the snapshot just written, so the
   file stays bounded by the edits since the last save. Caller holds
   journal-lock."
  [versions]
  (when-let [recs (seq (persist/read-lines journal-file))]
    (let [kept (into [] (filter (fn [r]
                                  (and (map? r) (integer? (:v r))
                                       (> (:v r) (get versions (:buf r) 0)))))
                     recs)]
      (if (seq kept)
        (persist/write-lines! journal-file kept)
        (persist/delete! journal-file)))))

(defn- write-session! [session n signatures]
  (locking write-lock
    (when (> n @written-seq)
      (let [ok? (persist/save-quiet! session-file session)]
        (when ok?
          (reset! written-seq n)
          (reset! last-snapshot signatures)
          (locking journal-lock
            (trim-journal! (update-vals (:bufs session) #(or (:version %) 0)))))
        ok?))))

(defn save-session!
  "Save current editor state to ~/.nesmacs/session.edn. Returns true when written.
   Called on clean exit and periodically."
  [state]
  (let [n (swap! write-seq inc)
        signatures (snapshot-signatures state)]
    (write-session! (session-data state) n signatures)))

(defn save-tick!
  "Editor-loop step: snapshot the session when the buffer/window structure
   changes (so a file opened a moment ago survives a crash) and every ~5s when
   it changed. Serialization and the disk write happen on a future, so a 10 MB
   buffer no longer stalls the command loop."
  [editor-atom]
  (let [s      @editor-atom
        signatures (snapshot-signatures s)
        last-saved @last-snapshot
        tick?  (>= (swap! save-counter inc) save-interval)]
    (when tick? (reset! save-counter 0))
    (when (and (or tick? (not= (:structure signatures) (:structure last-saved)))
               (not= signatures last-saved)
               (compare-and-set! save-pending? false true))
      (let [n (swap! write-seq inc)]
        (future
          (try (write-session! (session-data s) n signatures)
               (catch Exception e
                 (log/log "session save failed:" (.getMessage e)))
               (finally (reset! save-pending? false))))))))

;; --- Edit journal ---
;;
;; The snapshot above is periodic. Between snapshots every text edit appends a
;; small record here, so `kill -9` loses at most the command in flight instead
;; of the whole interval. A record is `{:buf name :v version :from :to :repl}`.

(defn journal-tick!
  "Write buffered edit records to ~/.nesmacs/journal.edn and clear the ones
   written. Called from the editor loop after each command; records added
   concurrently by nREPL keep a higher version and survive the clear."
  [editor-atom]
  (let [s    @editor-atom
        recs (vec (for [[name b] (:bufs s), r (:journal b)]
                    (assoc r :buf name)))]
    (when (and (seq recs)
               ;; Serialized with snapshot writes so a trim can't race an append.
               (locking journal-lock (persist/append-lines! journal-file recs)))
      (let [up-to (reduce (fn [m r] (assoc m (:buf r) (:v r))) {} recs)]
        (swap! editor-atom
               (fn [state]
                 (update state :bufs
                         (fn [bufs]
                           (reduce-kv
                             (fn [acc name b]
                               (if-let [v (get up-to name)]
                                 (assoc acc name
                                        (update b :journal
                                                (fn [js]
                                                  (vec (drop-while #(<= (:v %) v) js)))))
                                 acc))
                             bufs bufs)))))))))

(defn note-saved!
  "Record that `name`'s current text is now on disk. A later restore can load
   the file and skip journal records up to this version instead of replaying
   edits the file already contains."
  [name version]
  (when (and name version)
    (locking journal-lock
      (persist/append-lines! journal-file [{:buf name :v version :save true}]))))

(defn- save-version
  "Highest save marker for `name` in `records` (0 when none)."
  [records name]
  (reduce (fn [v r]
            (if (and (:save r) (= name (:buf r))) (max (long v) (long (:v r))) v))
          0 records))

(defn- apply-record
  [state {:keys [buf v from to repl save]}]
  (let [b (get-in state [:bufs buf])
        current (or (:version b) 0)]
    (cond
      (or (nil? b) save) state
      ;; A torn or hand-edited record must not throw inside buf/edit.
      (not (and (string? repl) (integer? from) (integer? to)
                (<= 0 (long from) (long to))))
      (do (log/log "journal record ignored for" buf) state)
      ;; Already part of the snapshot: the edit happened before it was written.
      (<= v current) state
      (= v (inc current))
      (update-in state [:bufs buf]
                 (fn [b] (-> (buf/edit b from to repl) (assoc :journal []))))
      :else
      (do (log/log "journal gap for" buf "at version" v) state))))

(defn recover-journal!
  "Replay the journal over a restored session and persist the result as the new
   base before deleting the journal. Returns the updated state."
  [state]
  (if-let [records (seq (filter #(and (map? %) (integer? (:v %)))
                                (persist/read-lines journal-file)))]
    (let [;; A buffer restored from disk holds text as of its last save, which may
          ;; be newer than the snapshot. Start replay from that save, or the
          ;; edits already in the file would be applied a second time.
          from-disk (filter (fn [[_ b]] (:journal-from-disk? b)) (:bufs state))
          bases (into {} (map (fn [[name b]]
                                [name (max (long (or (:version b) 0))
                                           (save-version records name))])
                              from-disk))
          state (update state :bufs
                        (fn [bufs]
                          (reduce-kv (fn [acc name b]
                                       (if (contains? bases name)
                                         (assoc acc name
                                                (-> b (assoc :version (bases name))
                                                    (dissoc :journal-from-disk?)))
                                         acc))
                                     bufs bufs)))
          s'    (reduce apply-record state records)]
      (if (save-session! s')
        (persist/delete! journal-file)
        (log/log "journal kept: session save failed"))
      s')
    state))

(defn discard-journal!
  "Drop the journal after a clean exit: the final snapshot already has it all."
  []
  (persist/delete! journal-file))

(defn sanitize-bookmarks
  "Keep bookmark entries whose key is a name and whose value has a buffer
   name and a position. Used for both session and standalone bookmark files."
  [m]
  (if (map? m)
    (into {}
          (filter (fn [[name v]]
                    (and (string? name) (string? (:buf v)) (nat-int? (:pos v)))))
          m)
    {}))

(defn sanitize-strings
  "Keep only the strings from a persisted sequence."
  [xs]
  (if (sequential? xs) (filterv string? xs) []))

(defn- window-tree?
  "A structurally valid window tree: leaves name a buffer, splits have both
   children. `win/leaves` recurses forever on a nil child, so a corrupt session
   file would otherwise hang startup."
  [node]
  (cond
    (win/leaf? node) (string? (:buffer node))
    (and (map? node) (= :split (:type node)))
    (and (window-tree? (:a node)) (window-tree? (:b node)))
    :else false))

(defn- sanitize-session
  "Validate and clean loaded session data."
  [data]
  (when (and (map? data)
             (= session-version (:version data)))
    (let [bufs (into {}
                     (filter (fn [[name b]] (and (string? name) (map? b))))
                     (when (map? (:bufs data)) (:bufs data)))
          windows (if (window-tree? (:windows data))
                    (:windows data)
                    (win/leaf (or (first (keys bufs)) "*scratch*")))
          cur-window (:cur-window data)]
      {:bufs bufs
       :windows windows
       :cur-window cur-window
       :recentf (sanitize-strings (:recentf data))
       :bookmarks (sanitize-bookmarks (:bookmarks data))
       :theme (when (keyword? (:theme data)) (:theme data))
       :breakpoints (when (map? (:breakpoints data)) (:breakpoints data))
       :mini-history (sanitize-strings (:mini-history data))})))

(defn load-session!
  "Load session from ~/.nesmacs/session.edn. Returns nil if no valid session."
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
          theme (:theme session-data)

          ;; Rebuild buffers from saved data
          rebuilt-bufs (update-vals bufs data->buffer)

          ;; Filter out buffers that no longer exist on disk (optional)
          existing-bufs (into {}
                          (filter (fn [[_ b]]
                                    (and b
                                         (or (:modified b)
                                             (nil? (:file b))
                                             (.exists (java.io.File. ^String (:file b))))))
                                rebuilt-bufs))
          ;; Every restored window leaf needs a buffer that exists. If all
          ;; files vanished (and *scratch* was killed), the renderer would
          ;; dereference nil before the first frame.
          valid-bufs (if (seq existing-bufs)
                       existing-bufs
                       {"*scratch*" (buf/make "*scratch*" "" nil)})
          ;; A leaf whose buffer vanished is rebuilt on some buffer that exists.
          fallback-buf (first (keys valid-bufs))

          ;; Ensure cur-window names a leaf that still exists.
          leaf-paths (win/leaves windows)
          valid-cur-window (if (some #(= cur-window %) leaf-paths)
                             cur-window
                             (first leaf-paths))

          ;; Rebuild window tree with actual buffer objects
          rebuilt-windows (letfn [(walk [node]
                                    (if (win/leaf? node)
                                      (let [buf-name (:buffer node)]
                                        (if (contains? valid-bufs buf-name)
                                          (let [b (get valid-bufs buf-name)
                                                w (win/leaf buf-name
                                                            (buf/clamp-pos b (when (integer? (:scroll node))
                                                                               (:scroll node)))
                                                            (max 0 (long (if (integer? (:hscroll node))
                                                                           (:hscroll node)
                                                                           0))))]
                                            (merge w (buf/clamp-window-points
                                                       b (select-keys node (filter #(integer? (get node %))
                                                                                   [:point :mark])))))
                                          (win/leaf fallback-buf)))
                                      (assoc (win/split-node (:dir node)
                                                             (walk (:a node))
                                                             (walk (:b node)))
                                             :ratio (if (number? (:ratio node))
                                                      (-> (double (:ratio node))
                                                          (max 0.1) (min 0.9))
                                                      0.5))))]
                            (walk windows))

          new-state (-> state
                        (assoc :bufs valid-bufs)
                        (assoc :windows rebuilt-windows)
                        (assoc :cur-window valid-cur-window)
                        (assoc :recentf recentf)
                        (assoc :bookmarks bookmarks)
                        (assoc :mini-history mini-history)
                        (assoc :theme (or theme (:theme state :default)))
                        ;; The window tree decides what is on screen, but keys and
                        ;; `cmd/cur` follow `:buf`. Leaving it at the initial
                        ;; `*scratch*` made a restored terminal look focused while
                        ;; keystrokes went to another buffer.
                        (assoc :buf (let [b (get-in rebuilt-windows
                                                    (conj valid-cur-window :buffer))]
                                      (if (contains? valid-bufs b)
                                        b
                                        fallback-buf))))]
      [new-state true])))
