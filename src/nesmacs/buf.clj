(ns nesmacs.buf
  (:require [clojure.string :as str]
            [nesmacs.gap :as gap]
            [nesmacs.props :as props]))

(defn detect-eol [^String s] (if (str/includes? s "\r\n") :crlf :lf))
(defn normalize-eol [^String s]
  (-> s (str/replace "\r\n" "\n") (str/replace "\r" "\n")))

(defn internal-name?
  "Leading space reserves a buffer name for editor internals."
  [name]
  (str/starts-with? name " "))

(def undo-limit 1000)

;; Large-file threshold: skip fontification and property work above this size.
;; 1 MB file or 100k lines.
(def ^:private large-file-bytes 1048576)
(def ^:private large-file-lines 100000)

(defn large-file?
  "Check if buffer should use large-file mode (skip fontification/props)."
  [b]
  (or (> (long (or (some-> ^String (:file b) java.io.File. .length) 0))
         large-file-bytes)
      (>= (gap/line-count (:text b)) large-file-lines)))

(defn bounded-conj
  "conj v onto vec, dropping the oldest entry once size exceeds limit."
  [vec limit v]
  (let [c (conj vec v)]
    (if (> (count c) limit) (subvec c 1) c)))

(defn make
  ([name] (make name "" nil))
  ([name text file] (make name text file nil))
  ([name text file line-ending]
   (let [b {:name name :text (gap/of text) :point 0 :mark nil :file file
            :modified false :mode :fundamental :undo [] :redo []
            :version 0 :journal? true :props [] :font-lock []
            :font-lock-window nil}]
     (cond-> (assoc b :large-file (large-file? b))
       line-ending (assoc :line-ending line-ending)))))

(defn clamp-pos
  "Clamp a text offset into the buffer [0, len]. Session restore uses it, since
   the file may have shrunk while the editor was closed."
  [b p]
  (-> (long (or p 0)) (max 0) (min (count (:text b)))))

(defn clamp-window-points [b window]
  (into {}
        (keep (fn [key]
                (when (contains? window key)
                  [key (when-let [p (get window key)] (clamp-pos b p))])))
        [:point :mark]))

(defn clamp-range
  "Clamp both endpoints to the buffer, keeping the end at or after the start."
  [b from to]
  (let [n (count (:text b))
        from (-> (long from) (max 0) (min n))]
    [from (-> (long to) (max from) (min n))]))

(defn set-point [b f]
  (assoc b :point (-> (f (:text b) (:point b))
                      (max 0) (min (count (:text b))))))

(defn- shift-pos
  "Shift a position to reflect an edit [from,to) → text of `repl-len` chars."
  [^long p ^long from ^long to ^long repl-len]
  (cond (< p from) p
        (<= p to)  (+ from repl-len)
        :else      (+ p (- repl-len (- to from)))))

(defn- truncate-font-lock
  "Drop font-lock entries at or beyond the edit line. Entry i holds the spans
   and lexer state *after* line i, so everything from the edited line down may
   change. Copy the kept prefix: a subvec would keep the dropped spans
   reachable, and these entries are much heavier than the old state cache."
  [font-lock from-line]
  (let [n (count (or font-lock []))
        keep (min n (max 0 from-line))]
    (if (zero? keep) [] (into [] (subvec font-lock 0 keep)))))

(defn- apply-change [b from to repl]
  (let [repl-len (count repl)
        new-text (gap/edit (:text b) from to repl)
        v (inc (or (:version b) 0))
        is-large? (:large-file b)]
    (-> b
        (assoc :text new-text :modified true :version v)
        (update :edit-count (fnil inc 0))
        (cond-> (not is-large?)
          (update :font-lock truncate-font-lock (gap/line-of new-text from)))
        ;; Props are annotations (few), not fontification, so keep them correct
        ;; even in large-file mode where they are not painted.
        (update :props (fnil props/adjust []) from to repl-len)
        (update :point shift-pos from to repl-len)
        (update :mark #(when % (shift-pos % from to repl-len)))
        ;; The editor loop drains this into ~/.nesmacs/journal.edn, so a kill -9
        ;; between session snapshots loses at most the command in flight.
        (cond-> (:journal? b)
          (update :journal (fnil conj []) {:v v :from from :to to :repl repl})))))

(defn- push-undo
  "Push an undo entry, coalescing adjacent pure insertions so typing is one
   undo step until a move or a non-insert edit. After an undo we always start a
   fresh entry, keeping redo boundaries meaningful."
  [undo entry had-redo?]
  (let [{pf :from pold :old pnew :new} (peek undo)]
    (if (and (not had-redo?) (not (:separate? entry)) (not (:separate? (peek undo)))
             (= "" (:old entry)) (= "" pold) (string? pnew)
             (string? (:new entry)) (not= "" (:new entry))
             (= (:from entry) (+ pf (count pnew))))
      (conj (pop undo) (assoc (peek undo) :new (str pnew (:new entry))))
      (bounded-conj undo undo-limit entry))))

(defn edit
  ([b from to repl] (edit b from to repl false))
  ([b from to repl separate-undo?]
   (let [[from to] (clamp-range b from to)]
     (if (and (= from to) (zero? (count repl)))
       b
       (let [old (if (= from to)
                   ""
                   (gap/substr (:text b) from to))
             had-redo? (seq (:redo b))
             entry (cond-> {:from from :old old :new repl}
                     separate-undo? (assoc :separate? true))]
         (-> (apply-change b from to repl)
             (update :undo push-undo entry had-redo?)
             (assoc :redo [])))))))

(defn replace-text
  "Replace the whole buffer through the normal undoable edit path."
  [b text]
  (edit b 0 (count (:text b)) text))

(defn- replay
  "Pop the latest entry from src-key, swap its `gone` text for its `kept` text,
   move point to the tail of the kept text, and push the entry onto dst-key."
  [b src-key dst-key gone-key kept-key]
  (if-let [{:keys [from] :as entry} (peek (get b src-key))]
    (let [to       (+ from (count (gone-key entry)))
          kept     (kept-key entry)
          kept-len (count kept)]
      (-> (apply-change b from to kept)
          (update src-key pop)
          (update dst-key bounded-conj undo-limit entry)
          (assoc :point (+ from kept-len))))
    b))

(defn undo [b] (replay b :undo :redo :new :old))
(defn redo [b] (replay b :redo :undo :old :new))
