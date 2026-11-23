(ns xmas.session
  "Session persistence: save/restore buffers, windows, and editor state.
   Undo history is explicitly NOT persisted (per roadmap)."
  (:require [xmas.buf :as buf]
            [xmas.log :as log]
            [xmas.persist :as persist]
            [xmas.window :as win]))

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
                      (.canRead (java.io.File. file)))]
    {:name (:name b)
     :text (when-not on-disk? (str (:text b)))
     :point (:point b)
     :mark (:mark b)
     :file file
     :modified (:modified b)
     :mode (:mode b)
     :version (:version b)
     :edit-count (:edit-count b 0)
     :font-lock (when-not on-disk?
                  (mapv (fn [entry] [nil (second entry)]) (:font-lock b)))
     :props (:props b [])
     :large-file (:large-file b)}))

(defn- file-text
  "Re-read a buffer's file for restore. nil when it can no longer be read, so
   the caller drops the buffer instead of presenting empty text for a real file."
  [^String path]
  (try (buf/normalize-eol (slurp path :encoding "UTF-8"))
       (catch Exception _ nil)))

(defn- data->buffer
  "Deserialize a buffer from session data. Returns nil if its file-backed text
   is missing from the session and can no longer be read from disk."
  [d]
  (let [file (:file d)
        saved (:text d)
        text (or saved (some-> file file-text))]
    (when (or text (nil? file))
      ;; Text reloaded from disk may have changed since the session was saved,
      ;; so cached fontification and props no longer line up. Fontification is
      ;; derived: it is reused only when the text came from the session, and
      ;; starts empty when the text is reloaded from disk. Props ride along
      ;; only with session text, since their offsets would otherwise be stale.
      (let [b (-> (buf/make (:name d) (or text "") file)
                  (assoc :modified (:modified d)
                         :mode (:mode d)
                         :version (:version d)
                         :edit-count (:edit-count d)
                         :journal-from-disk? (and (some? file) (nil? saved))
                         :font-lock (if saved (or (:font-lock d) []) [])
                         :props (if saved (:props d) [])
                         :large-file (:large-file d)))]
        ;; A file reloaded from disk may have shrunk, so the saved offsets can
        ;; point past the end.
        (cond-> (assoc b :point (buf/clamp-pos b (:point d)))
          (some? (:mark d)) (assoc :mark (buf/clamp-pos b (:mark d))))))))

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
(def ^:private last-sig (atom nil))
(def ^:private last-struct (atom nil))

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
         (:recentf s) (:bookmarks s) (:mini-history s)]))

(defn- structure-signature
  "The part of the session that must reach disk soon: which buffers exist and
   how the windows show them. An edit can be rebuilt from the journal, but a
   buffer opened after the last snapshot would otherwise vanish."
  [s]
  (hash [(mapv (fn [[name b]] [name (:file b)]) (:bufs s))
         (:windows s) (:cur-window s)]))

(defn- session-data [state]
  (let [bufs (:bufs state)]
    {:version session-version
     :timestamp (System/currentTimeMillis)
     :bufs (update-vals bufs buffer->data)
     :windows (window-tree->data (:windows state))
     :cur-window (:cur-window state)
     :recentf (:recentf state [])
     :bookmarks (:bookmarks state {})
     :mini-history (:mini-history state [])}))

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

(defn- write-session! [session n]
  (locking write-lock
    (when (> n @written-seq)
      (let [ok? (persist/save-quiet! session-file session)]
        (when ok?
          (reset! written-seq n)
          (locking journal-lock
            (trim-journal! (update-vals (:bufs session) #(or (:version %) 0)))))
        ok?))))

(defn save-session!
  "Save current editor state to ~/.xmas/session.edn. Returns true when written.
   Called on clean exit and periodically."
  [state]
  (write-session! (session-data state) (swap! write-seq inc)))

(defn save-tick!
  "Editor-loop step: snapshot the session when the buffer/window structure
   changes (so a file opened a moment ago survives a crash) and every ~5s when
   it changed. Serialization and the disk write happen on a future, so a 10 MB
   buffer no longer stalls the command loop."
  [editor-atom]
  (let [s      @editor-atom
        struct (structure-signature s)
        sig    (signature s)
        tick?  (>= (swap! save-counter inc) save-interval)]
    (when tick? (reset! save-counter 0))
    (when (and (or tick? (not= struct @last-struct))
               (or (not= sig @last-sig) (not= struct @last-struct))
               (compare-and-set! save-pending? false true))
      (reset! last-sig sig)
      (reset! last-struct struct)
      (let [n (swap! write-seq inc)]
        (future
          (try (let [data (session-data s)]
                 (write-session! data n))
               (catch Exception e
                 (log/log "session save failed:" (.getMessage e)))
               (finally (reset! save-pending? false))))))))

;; --- Edit journal ---
;;
;; The snapshot above is periodic. Between snapshots every text edit appends a
;; small record here, so `kill -9` loses at most the command in flight instead
;; of the whole interval. A record is `{:buf name :v version :from :to :repl}`.

(defn journal-tick!
  "Write buffered edit records to ~/.xmas/journal.edn and clear the ones
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
      (not (string? repl)) state
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

(defn- sanitize-session
  "Validate and clean loaded session data."
  [data]
  (when (and (map? data)
             (= session-version (:version data)))
    (let [bufs (:bufs data)
          windows (:windows data)
          cur-window (:cur-window data)]
      {:bufs bufs
       :windows windows
       :cur-window cur-window
       :recentf (sanitize-strings (:recentf data))
       :bookmarks (sanitize-bookmarks (:bookmarks data))
       :mini-history (sanitize-strings (:mini-history data))})))

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
                                    (and b
                                         (or (:modified b)
                                             (nil? (:file b))
                                             (.exists (java.io.File. (:file b))))))
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
                                          (let [b (get valid-bufs buf-name)
                                                w (win/leaf buf-name
                                                            (buf/clamp-pos b (:scroll node 0))
                                                            (max 0 (long (or (:hscroll node) 0))))]
                                            (cond-> w
                                              (some? (:point node))
                                              (assoc :point (buf/clamp-pos b (:point node)))
                                              (some? (:mark node))
                                              (assoc :mark (buf/clamp-pos b (:mark node)))))
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