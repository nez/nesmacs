(ns nesmacs.nav
  "Location listings that jump elsewhere: occur (regex hits in one buffer),
   imenu (definitions read from the tokenizer), and ripgrep result lines.

   The tokenizer has no parser behind it, so imenu only sees definitions whose
   keyword and name sit on one line — good enough for Clojure and Lisp."
  (:require [nesmacs.cmd :as cmd]
            [nesmacs.face :as face]
            [nesmacs.files :as files]
            [nesmacs.gap :as gap]
            [nesmacs.listbuf :as listbuf]))

;; --- Occur ---

(def ^:private max-occur 2000)

(defn occur-entries
  "One entry per regex match in `text`, capped so a pathological pattern can't
   build an unbounded list."
  [^String text pattern]
  (let [m (re-matcher (re-pattern pattern) text)]
    (loop [acc []]
      (if (or (>= (count acc) max-occur) (not (.find m)))
        acc
        (let [s  (.start m)
              ln (gap/line-of text s)
              ls (gap/nth-line-start text ln)
              le (gap/nth-line-end text ln)]
          (recur (conj acc {:line (inc ln) :text (gap/substr text ls le)})))))))

(defn- nav-buffer
  "Build a read-only *nav* list buffer whose entries point back at `source`."
  [name source header entries format-entry]
  (listbuf/make
    {:name         name
     :mode         :nav-mode
     :header       header
     :entries      (mapv #(assoc % :buf source) entries)
     :format-entry format-entry
     :extras       {:occur-source source}}))

(defn occur-buffer [name source pattern entries]
  (nav-buffer name source (str (count entries) " matches for " pattern) entries
              (fn [{:keys [line text]}] (format "%6d: %s" line text))))

;; --- Imenu ---

(def imenu-symbols face/definition-lines)

(defn imenu-buffer [name source symbols]
  (nav-buffer name source (str (count symbols) " symbols") symbols
              (fn [{:keys [name line]}] (format "%5d  %s" line name))))

;; --- Ripgrep output ---

(defn rg-line
  "Parse `path:line:text` (rg with --with-filename). Nil for other lines."
  [^String line]
  (when-let [[_ file ln] (re-find #"^([^:]+):(\d+):" line)]
    {:file file :line (Long/parseLong ln)}))

(defn- visit
  "Visit a location from a listing, language server, or debugger."
  [s root {:keys [file buf line character location-error]}]
  (let [position #(if line (cmd/goto-line % line (or character 0)) %)]
    (cond
      location-error (cmd/msg s location-error)
      file (let [f (java.io.File. ^String file)
                 f (if (or (nil? root) (.isAbsolute f)) f (java.io.File. ^String root file))
                 {:keys [path exists dir?] :as info} (files/file-info (.getPath f))]
             (if (or (files/file-buffer-key s path) (and exists (not dir?)))
               (let [visited (files/visit s info)]
                 (if (= path (:file (cmd/cur visited))) (position visited) visited))
               (cmd/msg s (str "Source file not found: " file))))
      buf (if (cmd/buf s buf)
            (position (cmd/set-cur-buffer s buf))
            (cmd/msg s "Source buffer is gone"))
      :else (cmd/msg s "This entry has no source location"))))

(defn- position [s]
  (let [{:keys [file text point]} (cmd/cur s)
        line (when text (gap/line-of text (or point 0)))]
    (cond-> {:file file :buf (:buf s)}
      line (assoc :line (inc line) :character (- (or point 0) (gap/nth-line-start text line))))))

(defn jump [s root entry]
  (let [origin (position s)
        dest (visit s root entry)]
    (if (= origin (position dest)) dest
        (-> dest
            (update :nav-back #(vec (take-last 100 (conj (or % []) origin))))
            (dissoc :nav-forward)))))

(defn- history [s from to]
  (if-let [destination (peek (from s))]
    (let [origin (position s)
          next (visit s nil destination)]
      (-> next (update from pop) (update to (fnil conj []) origin)))
    (cmd/msg s "No navigation history")))

(defn back [s] (history s :nav-back :nav-forward))
(defn forward [s] (history s :nav-forward :nav-back))

(defn jump-at-point [s]
  (let [{:keys [text mode nav-root job-dir]} (cmd/cur s)]
    (if-let [entry (listbuf/entry-at-point s)]
      (jump s nav-root entry)
      (if (= mode :rg-mode)
        (let [line (cmd/line-idx s)
              location (rg-line (gap/substr text (gap/nth-line-start text line)
                                               (gap/nth-line-end text line)))]
          (cond
            (nil? location) (cmd/msg s "No search result on this line")
            (or job-dir nav-root) (jump s (or job-dir nav-root) location)
            :else (cmd/msg s "Run project search again to open these results")))
        s))))
