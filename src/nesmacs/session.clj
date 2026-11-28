(ns nesmacs.session
  "Session persistence: save/restore buffers, windows, and editor state.
   Undo history is explicitly NOT persisted (per roadmap)."
  (:require [nesmacs.buf :as buf]
            [nesmacs.dap :as dap]
            [nesmacs.dired :as dired]
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
       :project-root (:project-root b)
       :dired-dir (:dired-dir b)
       :read-only (:read-only b)
       :journal? (:journal? b)
       :journal-id (:journal-id b)
       :line-ending (:line-ending b)
       :modified (:modified b)
       :disk-revision (:disk-revision b)
       :mode (:mode b)
       :version (:version b)
       :edit-count (:edit-count b 0)
       :font-lock (when-not on-disk?
                    (mapv (fn [entry] [nil (second entry)]) (:font-lock b)))
       :props (into [] (remove #(#{:isearch :qr :query-replace} (:owner %))) (:props b))
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

(defn- safe-int [x] (if (int? x) x 0))

(defn- data->buffer
  "Deserialize a buffer from session data. Returns nil if its file-backed text
   is missing from the session and can no longer be read from disk."
  [d]
  ;; A hand-edited or truncated session file must not crash startup: coerce
  ;; every field a later `long`/`File` call would choke on.
  (let [file (when (string? (:file d)) (:file d))
        saved (when (string? (:text d)) (:text d))
        term (sanitize-terminal (:terminal d))
        stored-revision (:disk-revision d)
        stored-revision (when (or (= :missing stored-revision) (string? stored-revision)) stored-revision)
        disk (when (and file (or (nil? saved) (nil? stored-revision) (nil? (:line-ending d))))
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
                         :project-root (when (string? (:project-root d)) (:project-root d))
                         :read-only (true? (:read-only d))
                         :journal? (not (false? (:journal? d)))
                         :journal-id (when (uuid? (:journal-id d)) (:journal-id d))
                         :disk-revision (if (nil? saved) (:revision disk) (or stored-revision (:revision disk)))
                         :saved-revision (when-not (:modified d) (safe-int (:version d)))
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
            (int? (:mark d)) (assoc :mark (buf/clamp-pos b (:mark d)))
            (coll? (:collapsed d))
            (assoc :collapsed
                   (into #{} (filter #(and (int? %)
                                           (< -1 (long %) (gap/line-count (:text b)))))
                         (:collapsed d)))))))))

(defn- restore-buffer [data]
  (if-let [dir (when (= :dired-mode (:mode data))
                 (or (when (string? (:dired-dir data)) (:dired-dir data))
                     (second (re-matches #"(?s)\*dired (.+)\*" (:name data)))))]
    (try (let [b (dired/directory-buffer dir)]
           (assoc b :point (buf/clamp-pos b (safe-int (:point data)))))
         (catch Exception e
           (log/log "directory restore failed:" (.getMessage e))
           (data->buffer (assoc data :read-only true :journal? false))))
    (data->buffer data)))

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

(defn- session-buffers [s]
  (remove (fn [[name _]] (buf/internal-name? name)) (:bufs s)))

(defn signature
  "Cheap change signature: buffer versions/points and layout, not buffer text.
   Lets the periodic save skip idle ticks without re-serializing 10 MB buffers."
  [s]
  (hash [(mapv (fn [[name b]]
                 [name (:journal-id b) (:version b) (:point b) (:modified b)])
               (session-buffers s))
         (:windows s) (:cur-window s)
         (:project-root s) (:project-recent s) (:project-views s)
         (:recentf s) (:bookmarks s) (:mini-history s) (:theme s :default)
         (dap/snapshot-breakpoints)]))

(defn- structure-signature
  "The part of the session that must reach disk soon: which buffers exist and
   how the windows show them. An edit can be rebuilt from the journal, but a
   buffer opened after the last snapshot would otherwise vanish."
  [s]
  (hash [(mapv (fn [[name b]] [name (when (:journal? b) (:journal-id b)) (:file b)]) (session-buffers s))
         (:windows s) (:cur-window s)]))

(defn- snapshot-signatures [state]
  {:signature (signature state) :structure (structure-signature state)})

(defn- session-data [state]
  {:version session-version
     :timestamp (System/currentTimeMillis)
     :bufs (into {} (map (fn [[name b]] [name (buffer->data b)])) (session-buffers state))
     :windows (window-tree->data (:windows state))
     :cur-window (:cur-window state)
     :project-root (:project-root state)
     :project-recent (:project-recent state)
     :project-views (:project-views state)
     :recentf (:recentf state [])
     :bookmarks (:bookmarks state {})
     :mini-history (:mini-history state [])
     :theme (:theme state :default)
     :breakpoints (dap/snapshot-breakpoints)})

(defn- journal-key [name b] [name (:journal-id b)])

(defn- trim-journal!
  "Drop journal records already covered by the snapshot just written, so the
   file stays bounded by the edits since the last save. Caller holds
   journal-lock."
  [bufs]
  (when-let [recs (seq (persist/read-lines journal-file))]
    (let [kept (filterv (fn [r]
                          (let [b (get bufs (:buf r))]
                            (and (map? r) (int? (:v r))
                                 (or (nil? b) (not= (:journal-id r) (:journal-id b))
                                     (> (:v r) (:version b 0))))))
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
            (trim-journal! (:bufs session))))
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
;; of the whole interval. Records identify both a buffer name and its instance.

(defn journal-tick!
  "Write buffered edit records to ~/.nesmacs/journal.edn and clear the ones
   written. Called from the editor loop after each command; records added
   concurrently by nREPL keep a higher version and survive the clear."
  [editor-atom]
  (let [s    @editor-atom
        recs (vec (for [[name b] (session-buffers s), r (:journal b)]
                    (assoc r :buf name :journal-id (:journal-id b))))]
    (when (and (seq recs)
               ;; Serialized with snapshot writes so a trim can't race an append.
               (locking journal-lock (persist/append-lines! journal-file recs)))
      (let [up-to (into {} (map (fn [r] [(journal-key (:buf r) r) (:v r)])) recs)]
        (swap! editor-atom
               (fn [state]
                 (update state :bufs
                         (fn [bufs]
                           (reduce-kv
                             (fn [acc name b]
                               (if-let [v (get up-to (journal-key name b))]
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
  [name {:keys [version journal-id]}]
  (when (and name version)
    (locking journal-lock
      (persist/append-lines! journal-file [{:buf name :journal-id journal-id :v version :save true}]))))

(defn- save-versions [records]
  (reduce (fn [saved r]
            (if (:save r)
              (update saved (journal-key (:buf r) r) (fnil max 0) (:v r))
              saved))
          {} records))

(defn- apply-record
  [state {:keys [buf v from to repl save journal-id]}]
  (let [b (get-in state [:bufs buf])
        current (or (:version b) 0)]
    (cond
      (or (nil? b) save (not= journal-id (:journal-id b))) state
      ;; A torn or hand-edited record must not throw inside buf/edit.
      (not (and (string? repl) (int? from) (int? to)
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
  (if-let [records (seq (filter #(and (map? %) (int? (:v %)))
                                (persist/read-lines journal-file)))]
    (let [;; A buffer restored from disk holds text as of its last save, which may
          ;; be newer than the snapshot. Start replay from that save, or the
          ;; edits already in the file would be applied a second time.
          saved (save-versions records)
          state (update state :bufs
                        (fn [bufs]
                          (into {} (for [[name b] bufs]
                                     [name (if (:journal-from-disk? b)
                                             (let [version (max (:version b 0) (get saved (journal-key name b) 0))]
                                               (-> b (assoc :version version :saved-revision version)
                                                   (dissoc :journal-from-disk?)))
                                             b)]))))
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
       :project-root (when (string? (:project-root data)) (:project-root data))
       :project-recent (vec (take 40 (sanitize-strings (:project-recent data))))
       :project-views (into {} (filter (fn [[root view]]
                                       (and (string? root) (map? view) (window-tree? (:windows view)))))
                            (when (map? (:project-views data)) (:project-views data)))
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

(defn- restore-window [node bufs fallback]
  (if (win/leaf? node)
    (let [name (:buffer node)]
      (if-let [b (get bufs name)]
        (merge (win/leaf name (buf/clamp-pos b (safe-int (:scroll node)))
                         (max 0 (safe-int (:hscroll node))))
               (buf/clamp-window-points b (into {} (filter (comp int? val))
                                               (select-keys node [:point :mark]))))
        (win/leaf fallback)))
    (let [ratio (:ratio node)]
      (assoc (win/split-node (:dir node)
                             (restore-window (:a node) bufs fallback)
                             (restore-window (:b node) bufs fallback))
             :ratio (if (and (number? ratio) (Double/isFinite (double ratio)))
                      (-> (double ratio) (max 0.1) (min 0.9)) 0.5)))))

(defn restore-session
  "Apply loaded session data to a fresh editor state."
  [state {:keys [windows cur-window theme] :as session-data}]
  (when session-data
    (let [restored (into {}
                         (keep (fn [[name data]]
                                 (when-let [b (restore-buffer (update data :name #(if (string? %) % name)))]
                                   [name b])))
                         (session-buffers session-data))
          bufs (if (seq restored) restored {"*scratch*" (buf/make "*scratch*")})
          windows (restore-window windows bufs (first (keys bufs)))
          paths (win/leaves windows)
          path (or (some #{cur-window} paths) (first paths))]
      [(-> state
           (merge (select-keys session-data [:recentf :bookmarks :mini-history :project-root :project-recent]))
           (assoc :project-views
                  (update-vals (:project-views session-data {})
                               (fn [{:keys [windows cur-window]}]
                                 (let [tree (restore-window windows bufs (first (keys bufs)))
                                       paths (win/leaves tree)]
                                   {:windows tree :cur-window (or (some #{cur-window} paths) (first paths))}))))
           (assoc :bufs bufs :windows windows :cur-window path
                  :theme (or theme (:theme state :default))
                  :buf (get-in windows (conj path :buffer))))
       true])))
