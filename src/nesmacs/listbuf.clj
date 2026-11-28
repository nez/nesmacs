(ns nesmacs.listbuf
  "A read-only buffer rendered as a header plus one line per entry, with the
   entry vector cached on the buffer for O(1) line→entry lookup. Used by
   dired and buflist; any read-only listing should reach for it instead of
   reimplementing the line-to-entry math."
  (:require [clojure.string :as str]
            [nesmacs.buf :as buf]
            [nesmacs.cmd :as cmd]))

(defn make
  "Build a list-buffer from a spec.
   :name           buffer name
   :mode           mode keyword (e.g. :dired-mode)
   :header         line 0
   :entries        rich records, one per subsequent line
   :format-entry   (fn [entry] line-string)
   :version        optional refresh counter
   :extras         optional map merged onto the buffer"
  [{:keys [name mode header entries format-entry version extras]
    :or   {version 0 extras {}}}]
  (let [text (str/join "\n" (cons header (map format-entry entries)))]
    (-> (buf/make name text nil)
        (assoc :journal? false)
        (assoc :mode mode :read-only true
               :listbuf-entries (vec entries) :version version)
        (merge extras))))

(defn entry-at-point
  "The entry at point, accounting for the 1-line header. Nil on the header."
  [s]
  (let [idx (dec (cmd/line-idx s))]
    (when (>= idx 0) (get (:listbuf-entries (cmd/cur s)) idx))))

(defn- install [s name b]
  (let [old (cmd/buf s name)]
    (assoc-in s [:bufs name]
              (cond-> b old (update :version max (inc (:version old 0)))))))

(defn refresh
  "Replace `name` with a fresh make from `spec`, preserving point clamped
   to the new text length and bumping the version."
  [s name spec]
  (let [fresh (make spec)
        point (:point (cmd/buf s name) 0)]
    (install s name (assoc fresh :point (buf/clamp-pos fresh point)))))

(defn show
  "Install an already built list buffer and show it in the current window.
   An open minibuffer keeps focus: a job that finishes mid-prompt must not
   replace its input."
  [s name b]
  (-> s (install name b) (cmd/set-cur-buffer name)))

(defn open
  "Build `spec` and show it (see `show`)."
  [s name spec]
  (show s name (make spec)))

(defn reload
  "Refresh `name` from `spec`; when `ok?` is false, also report `missing-msg`
   (the listing keeps whatever it could render)."
  [s name spec ok? missing-msg]
  (cond-> (refresh s name spec)
    (not ok?) (cmd/msg missing-msg)))
