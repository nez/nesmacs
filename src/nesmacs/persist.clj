(ns nesmacs.persist
  "Read/write small EDN documents under ~/.nesmacs/. Used by bookmarks,
   recentf, and mini-history. Failures are logged and return defaults —
   persistence is convenience, not correctness, and a corrupt or missing
   file should never prevent the editor from starting."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [nesmacs.log :as log])
  (:import [java.io PushbackReader]
           [java.nio.file Files StandardCopyOption]))

(def ^:dynamic *dir*
  "Override the persistence directory; tests bind it to a temp dir."
  nil)

(defn- nesmacs-dir ^java.io.File []
  (or *dir* (io/file (System/getProperty "user.home") ".nesmacs")))

(defn- file ^java.io.File [^String name]
  (io/file (nesmacs-dir) (str name ".edn")))

(defn- ensure-dir! [^java.io.File d]
  (when (and d (not (.exists d)))
    (.mkdirs d)))

(defn atomic-spit
  "Write `s` to `path` via a sibling temp file and rename, so a crash mid-write
   can't truncate the target. Preserves POSIX permissions; removes the temp on
   failure so a read-only dir or full disk leaves no debris."
  [^String path ^String s]
  (let [target (.toPath (.getAbsoluteFile (java.io.File. path)))
        parent (.getParent target)
        tmp    (Files/createTempFile parent ".nesmacs-" ".tmp"
                 (into-array java.nio.file.attribute.FileAttribute []))
        nolink (into-array java.nio.file.LinkOption [])
        done?  (atom false)]
    (try
      (spit (.toFile tmp) s :encoding "UTF-8")
      (when (Files/exists target nolink)
        (try (Files/setPosixFilePermissions tmp (Files/getPosixFilePermissions target nolink))
             (catch UnsupportedOperationException _)))
      (Files/move tmp target
        (into-array [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING]))
      (reset! done? true)
      true
      (finally
        (when-not @done?
          (try (Files/deleteIfExists tmp) (catch Exception _)))))))

(defn load!
  "Read EDN from name; return default on any failure."
  ([name]         (load! name nil))
  ([name default]
   (let [f (file name)]
     (if (.exists f)
       (try
         (with-open [r (PushbackReader. (io/reader f))]
           (edn/read {:default (fn [_ v] v)} r))
         (catch Exception e
           (log/log "persistence read failed:" name (.getMessage e))
           default))
       default))))

(defn save!
  "Write `value` to name as EDN atomically. Creates the directory if missing.
   Errors propagate, which is fine — callers that want silence wrap in try."
  [name value]
  (ensure-dir! (nesmacs-dir))
  (atomic-spit (str (file name)) (pr-str value)))

(defn save-quiet!
  "Like save! but logs and returns nil on IO errors (the common case for
   autosaving convenience data on every change)."
  [name value]
  (try (save! name value)
       (catch Exception e
         (log/log "persistence write failed:" name (.getMessage e))
         nil)))

(defn append-lines!
  "Append EDN values to `name`, one per line. Returns true when written."
  [name values]
  (let [f (file name)]
    (try
      (ensure-dir! (.getParentFile f))
      ;; create through atomic-spit so a fresh journal gets the same
      ;; owner-only permissions as the other persisted files
      (when-not (.exists f) (atomic-spit (.getPath f) ""))
      (spit f (apply str (map #(str (pr-str %) "\n") values))
            :append true :encoding "UTF-8")
      true
      (catch Exception e
        (log/log "persistence append failed:" name (.getMessage e))
        false))))

(defn write-lines!
  "Atomically replace `name` with `values`, one EDN value per line."
  [name values]
  (try
    (let [f (file name)]
      (ensure-dir! (.getParentFile f))
      (atomic-spit (.getPath f) (apply str (map #(str (pr-str %) "\n") values))))
    (catch Exception e
      (log/log "persistence write failed:" name (.getMessage e))
      false)))

(defn read-lines
  "Read the EDN values in `name`, skipping corrupt lines (a killed process can
   leave the last line torn)."
  [name]
  (let [f (file name)]
    (when (.exists f)
      (try
        (with-open [r (io/reader f :encoding "UTF-8")]
          (doall (keep (fn [line]
                         (try (edn/read-string line)
                              (catch Exception _ nil)))
                       (line-seq r))))
        (catch Exception e
          (log/log "persistence read failed:" name (.getMessage e))
          nil)))))

(defn delete! [name]
  (try (.delete (file name)) (catch Exception _ nil)))
