(ns nesmacs.files
  "Visit, save and recover editor documents."
  (:require [clojure.string :as str]
            [nesmacs.api :as api]
            [nesmacs.buf :as buf]
            [nesmacs.cmd :as cmd]
            [nesmacs.dired :as dired]
            [nesmacs.format :as format]
            [nesmacs.gap :as gap]
            [nesmacs.lang :as lang]
            [nesmacs.log :as log]
            [nesmacs.mini :as mini]
            [nesmacs.mode :as mode]
            [nesmacs.persist :as persist]
            [nesmacs.session :as session]))

(defn- auto-save-path
  "#file# backup path for a buffer file."
  [^String file]
  (let [f (java.io.File. file)
        parent (or (.getParent f) ".")]
    (str parent "/#" (.getName f) "#")))

(defn- auto-save-file ^java.io.File [^String file]
  (java.io.File. (auto-save-path file)))

(def ^:private auto-save-threshold 300)

(defn- auto-save-due
  "Pure: return [[buf-name file text version] ...] for every buffer at or above the
   auto-save threshold."
  [s]
  (keep (fn [[name b]]
          (when (and (:file b) (>= (or (:edit-count b) 0) auto-save-threshold))
            [name (:file b) (str (:text b)) (:version b)]))
        (:bufs s)))

(defn- reset-edit-counts
  "Pure: reset :edit-count to 0 for each buf still at or above threshold.
   Guards against racing edits: a concurrent save-buffer that already
   zeroed the counter is respected."
  [s due]
  (reduce (fn [s [name _ _ version]]
            (cond-> s
              (and (= version (get-in s [:bufs name :version]))
                   (>= (or (get-in s [:bufs name :edit-count]) 0)
                       auto-save-threshold))
              (assoc-in [:bufs name :edit-count] 0)))
          s due))

(defn- spit-due!
  "Write each [_ file text version] tuple to its auto-save path. Returns the
   entries written successfully and logs failures.
   Uses atomic-spit so a crash mid-write can't leave the backup truncated —
   the whole point of the backup is to be usable after unclean shutdown."
  [due]
  (keep (fn [[_ file text :as entry]]
          (try
            (persist/atomic-spit (auto-save-path file) text)
            entry
            (catch Exception e
              (log/log "auto-save failed:" file (.getMessage e))
              nil)))
        due))

(defn- remember-auto-saves!
  "Persist the set of files with a live `#file#` backup, so the next start can
   offer recovery. Entries whose backup is gone are pruned."
  [saved]
  (when (seq saved)
    (let [paths (->> (concat (persist/load! "auto-saved" [])
                             (map second saved))
                     (filter string?)
                     distinct
                     (filter #(.exists (auto-save-file %)))
                     vec)]
      (persist/save-quiet! "auto-saved" paths))))

(defn- save-backups! [state]
  (let [saved (spit-due! (auto-save-due state))]
    (remember-auto-saves! saved)
    saved))

(defn auto-save!
  "Write due autosaves for a state. Call outside swap!."
  [state]
  (reset-edit-counts state (save-backups! state)))

(defn auto-save-tick! [editor]
  (when-let [saved (seq (save-backups! @editor))]
    (swap! editor reset-edit-counts saved)))

(defn cli-formatted-text [{:keys [language text file]}]
  (some-> (format/format-text language (str text) file
                             (some-> ^String file java.io.File. .getParent))
          buf/normalize-eol))

(defn same-buffer-version? [b saved]
  (and (= (:file b) (:file saved))
       (= (:version b) (:version saved))
       (identical? (:text b) (:text saved))))

(defn write-buffer! [publish! name b expected]
  (let [revision (persist/write-file! (:file b) (str (:text b)) (:line-ending b) expected)
        _ (session/note-saved! name (:version b))
        state (publish! (fn [s]
                         (let [current (cmd/buf s name)]
                           (if (or (same-buffer-version? current b)
                                   (and (= (:file current) (:file b))
                                        (= (:disk-revision current) (:disk-revision b))
                                        (> (:version current 0) (:version b 0))))
                             (assoc-in s [:bufs name] (assoc (buf/mark-saved current b) :disk-revision revision))
                             s))))]
    (same-buffer-version? (cmd/buf state name) b)))

(defn save-buffer-named
  "Queue a save of buffer `name` (a :bufs key). Formatting and disk writes run
   after the command commits."
  [s name]
  (let [b (cmd/buf s name)
        file (:file b)
        format? (:format-on-save s)]
    (cond
      (nil? b) (cmd/msg s (str "No buffer: " name))
      (str/blank? file) (cmd/msg s "No file")
      :else
      (api/after-command s
        (fn [publish!]
          (try
            (let [to-save
                  (if-let [fmt (when format? (cli-formatted-text b))]
                    (let [state (publish! (fn [state]
                                            (if (same-buffer-version? (cmd/buf state name) b)
                                              (update-in state [:bufs name] buf/replace-text fmt)
                                              state)))
                          formatted (cmd/buf state name)]
                      (when (and (= file (:file formatted)) (= fmt (str (:text formatted))))
                        formatted))
                    b)]
              (if to-save
                (do
                  (when (write-buffer! publish! name to-save
                                       (or (:disk-revision b) (persist/file-revision file)))
                    (try
                      (java.nio.file.Files/deleteIfExists (.toPath (auto-save-file file)))
                      (catch Exception e
                        (log/log "auto-save cleanup failed:" file (.getMessage e)))))
                  (publish! cmd/msg (str "Wrote " file)))
                (publish! cmd/msg "Save canceled: buffer changed during formatting")))
            (catch Exception e (publish! cmd/msg-error "Save failed" e))))))))

(defn save-buffer [s] (save-buffer-named s (:buf s)))

(defn open-buf [s path b]
  (let [b (lang/with-file-language b)
        s (-> s (assoc-in [:bufs path] b) (cmd/set-cur-buffer path))
        m (get-in b [:language :mode])]
    (cond-> s m (mode/set-major-mode m))))

(defn file-info [filename]
  (let [f (.getAbsoluteFile (java.io.File. filename))]
    {:path (.getCanonicalPath f) :name (.getName f)
     :exists (.exists f) :dir? (.isDirectory f)}))

(def recentf-limit 50)

(defn bump-recentf [s path]
  (-> s
      (assoc :recentf (vec (take recentf-limit
                                (cons path (remove #{path} (:recentf s))))))
      (api/persist-setting :recentf)))

(defn file-buffer-key [s path]
  (or (when (contains? (:bufs s) path) path)
      (some (fn [[key b]] (when (= (:file b) path) key)) (:bufs s))))

(defn read-document [name path]
  (let [{:keys [revision text line-ending]} (persist/read-file path)]
    (assoc (buf/make name text path line-ending) :disk-revision revision)))

(defn visit [s {:keys [path name exists dir?]}]
  (let [key (file-buffer-key s path)]
    (cond
      ;; already open — just switch to it, don't clobber in-memory edits
      key (-> s (cmd/set-cur-buffer key) (bump-recentf path))

      ;; Opening a directory drops into dired, matching Emacs's C-x C-f.
      dir? (dired/open s path)

      exists
      (try (-> (open-buf s path (read-document name path))
               (bump-recentf path))
           (catch Exception e (cmd/msg-error s (str "Error reading " path) e)))

      :else (-> (open-buf s path (assoc (buf/make name "" path) :disk-revision :missing))
                (bump-recentf path)
                (cmd/msg "(New file)")))))

(defn find-file [s filename]
  (visit s (file-info filename)))

;; --- Crash recovery ---

(defn pending-recoveries
  "Files with an auto-save backup left by a previous run."
  []
  (->> (persist/load! "auto-saved" [])
       (filter string?)
       distinct
       (filter #(.exists (auto-save-file %)))
       vec))

(defn- recover-file
  "Open `f` and replace its text with the auto-save backup, leaving it marked
   modified so the user can inspect it before C-x C-s."
  [s f]
  (let [backup (auto-save-file f)]
    (if (.exists backup)
      (let [text (slurp backup :encoding "UTF-8")
            s    (find-file s f)]
        (-> (cmd/update-cur s #(assoc % :text (gap/of text) :point 0
                                        :modified true
                                        :revision nil
                                        ;; The backup is different text: cached
                                        ;; spans, props and undo offsets from the
                                        ;; file no longer describe it.
                                        :version (inc (or (:version %) 0))
                                        :props [] :font-lock [] :font-lock-window nil
                                        :undo [] :redo [] :journal []))
            (cmd/msg (str "Recovered " f " from auto-save — C-x C-s to keep"))))
      (cmd/msg s (str "No auto-save for " f)))))

(defn recover-ask
  "Prompt for each pending backup. `y` restores it, `n` deletes it."
  [s paths]
  (if-let [[f & more] (seq paths)]
    (mini/start s (str "Recover unsaved changes to " f "? (y/n) ")
      (fn [s ans]
        (case (str/lower-case (str/trim ans))
          "y" (recover-ask (recover-file s f) more)
          "n" (-> s
                  (api/after-command (fn [_]
                                       (java.nio.file.Files/deleteIfExists (.toPath (auto-save-file f)))))
                  (recover-ask more))
          (recover-ask s paths))))
    s))

;; Scratch and other non-file buffers are kept in the session.

(defn- modified-file [s name]
  (let [{:keys [file modified]} (cmd/buf s name)]
    (when modified file)))

(defn- quit-finish [s] (assoc s :exit true))

(defn- quit-save-all
  "Save the remaining modified files, then quit if every save succeeded."
  [s names]
  (let [files (filter #(modified-file s %) names)]
    (-> (reduce save-buffer-named s files)
        (api/after-command
          (fn [publish!]
            (publish! (fn [s]
                        (if (some #(:modified (cmd/buf s %)) files)
                          (cmd/msg s "Quit canceled: a buffer could not be saved")
                          (quit-finish s)))))))))


(defn- quit-ask [s names]
  (if-let [[name & more] (seq (filter #(modified-file s %) names))]
    (let [file (modified-file s name)]
      (mini/start s (str "Save " file "? (y/n/!) ")
        (fn [s ans]
          (case (str/lower-case (str/trim ans))
            "y" (-> (save-buffer-named s name)
                    (api/after-command
                      (fn [publish!]
                        (publish! (fn [s]
                                    (if (modified-file s name)
                                      (cmd/msg s (str "Quit canceled: " file " was not saved"))
                                      (quit-ask s more)))))))
            "n" (quit-ask s more)
            "!" (quit-save-all s (cons name more))
            (cmd/msg s "Quit canceled")))))
    (quit-finish s)))

(defn confirm-quit [s]
  (quit-ask (dissoc s :exit-pending :exit) (sort (keys (:bufs s)))))
