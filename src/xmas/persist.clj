(ns xmas.persist
  "Read/write small EDN documents under ~/.xmas/. Used by bookmarks,
   recentf, and mini-history. Failures are logged and return defaults —
   persistence is convenience, not correctness, and a corrupt or missing
   file should never prevent the editor from starting."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [xmas.log :as log])
  (:import [java.io PushbackReader]
           [java.nio.file Files StandardCopyOption]))

(defn- xmas-dir ^java.io.File []
  (io/file (System/getProperty "user.home") ".xmas"))

(defn- file ^java.io.File [^String name]
  (io/file (xmas-dir) (str name ".edn")))

(defn atomic-spit
  "Write `s` to `path` via a sibling temp file and rename, so a crash mid-write
   can't truncate the target. Preserves POSIX permissions; removes the temp on
   failure so a read-only dir or full disk leaves no debris."
  [^String path ^String s]
  (let [target (.toPath (.getAbsoluteFile (java.io.File. path)))
        parent (.getParent target)
        tmp    (Files/createTempFile parent ".xmas-" ".tmp"
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
  (let [d (xmas-dir)]
    (when-not (.exists d) (.mkdirs d)))
  (atomic-spit (str (file name)) (pr-str value)))

(defn save-quiet!
  "Like save! but logs and returns nil on IO errors (the common case for
   autosaving convenience data on every change)."
  [name value]
  (try (save! name value)
       (catch Exception e
         (log/log "persistence write failed:" name (.getMessage e))
         nil)))
