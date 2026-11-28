(ns nesmacs.complete
  "Asynchronous completion sources.

   A source is `(fn [prefix ctx] -> seq of candidate strings)`; ctx carries a
   snapshot of the editor state. `start!` runs every source on its own future,
   and `tick!` (called from the editor loop, never inside a swap!) moves
   finished candidates into the minibuffer. A request id keeps results from an
   old prefix out of the current list.

   Project file lists and project symbols are cached per root; a stale cache is
   dropped after `cache-ttl-ms` or on demand with `invalidate-cache!`."
  (:require [clojure.string :as str]
            [nesmacs.buf :as buf]
            [nesmacs.cmd :as cmd]
            [nesmacs.gap :as gap]
            [nesmacs.face :as face]
            [nesmacs.project :as project]))

;; --- Sources ---

(defn prefix-match?
  "Match a prefix case-insensitively while keeping the candidate's spelling."
  [^String prefix ^String candidate]
  (.regionMatches candidate true 0 prefix 0 (.length prefix)))

(defn document-items [buffer position]
  (let [cache (::document buffer)]
    (when (= (:version buffer) (:version cache))
      (get-in cache [:items position]))))

(defn document-candidates!
  "Cache server replies on the source buffer. A closed or edited buffer cannot
   accept an earlier reply, even when reopened at the same version."
  [editor state position prefix request!]
  (let [name (:buf state)
        source (cmd/cur state)
        version (:version source)
        base [:bufs name ::document]
        pending (into base [:pending position])
        ticket {:started (System/currentTimeMillis)}
        updated (swap! editor
                  (fn [s]
                    (let [b (cmd/buf s name)
                          active (get-in s pending)]
                      (if (or (not (identical? (:text source) (:text b)))
                              (not= version (:version b))
                              (document-items b position)
                              (and (= version (get-in s (conj base :version)))
                                   active (< (- (:started ticket) (:started active)) 5000)))
                        s
                        (-> s
                            (cond-> (not= version (get-in s (conj base :version)))
                              (assoc-in base {:version version}))
                            (assoc-in pending ticket))))))]
    (when (identical? ticket (get-in updated pending))
      (request!
        (fn [result]
          (let [items (if (map? result) (:items result) result)
                labels (into {} (keep #(when (:label %) [(:label %) %])) items)]
            (swap! editor
                   (fn [s]
                     (if (and (= version (get-in s [:bufs name :version]))
                              (identical? ticket (get-in s pending)))
                       (-> s
                           (update-in (conj base :pending) dissoc position)
                           (assoc-in (into base [:items position]) labels)
                           (cond-> (:mini s) (assoc-in [:mini :for] ::stale)))
                       s)))))))
    (filterv #(prefix-match? prefix %) (keys (document-items (cmd/buf updated name) position)))))

(defn- match? [prefix s]
  (str/includes? (str/lower-case s) (str/lower-case prefix)))

(defn buffers [prefix {:keys [state]}]
  (->> (keys (:bufs state))
       (remove buf/internal-name?)
       (filter #(match? prefix %))
       (sort)
       (take 200)
       vec))

(defn- symbols-in [^String text]
  (distinct (re-seq #"[A-Za-z_][A-Za-z0-9_\-!?*+]*" text)))

(defn buffer-symbols [prefix {:keys [state]}]
  (->> (symbols-in (str (:text (cmd/cur state))))
       (filter #(prefix-match? prefix %))
       (sort)
       (take 300)
       vec))

(defn- project-root [state]
  (project/context state))

;; --- Cache ---

(defonce ^:private cache (atom {}))
(def ^:private cache-ttl-ms 30000)

(defn invalidate-cache! [] (reset! cache {}))

(defn- cached [k build]
  (let [{:keys [stamp value]} (get @cache k)
        now (System/currentTimeMillis)]
    (if (and stamp (< (- now stamp) cache-ttl-ms))
      value
      (let [v (build)]
        (swap! cache assoc k {:stamp now :value v})
        v))))

(defn- root-files [root]
  (cached [:files root] #(project/files root)))

(defn- lisp-file? [path]
  (boolean (re-find #"\.(clj|cljc|cljs|edn|el)$" path)))

(defn- file-mode [path]
  (if (str/ends-with? path ".el") :emacs-lisp-mode :clojure-mode))

(defn- root-symbols [root]
  (cached [:symbols root]
    (fn []
      (->> (root-files root)
           (filter lisp-file?)
           (take 2000)
           (mapcat (fn [rel]
                     (let [f (java.io.File. ^String root ^String rel)]
                       (when (and (.isFile f) (< (.length f) 200000))
                         (map :name (face/definition-lines {:mode (file-mode rel)}
                                                       (slurp f)))))))
           (remove nil?)
           (distinct)
           vec))))

(defn project-files [prefix {:keys [state]}]
  (when-let [root (project-root state)]
    (->> (root-files root)
         (filter #(match? prefix %))
         (take 300)
         vec)))

(defn project-symbols [prefix {:keys [state]}]
  (when-let [root (project-root state)]
    (->> (root-symbols root)
         (filter #(prefix-match? prefix %))
         (take 300)
         vec)))

(def ^:private default-sources [buffers buffer-symbols])

(defn sources-for
  "Sources registered for the current buffer's mode, else the global default."
  [s]
  (or (get-in s [:completion-sources (:mode (cmd/cur s))])
      (get-in s [:completion-sources :default])
      default-sources))

(defn set-input
  "Replace minibuffer text and put point at the end."
  [s input]
  (cmd/update-cur s #(assoc % :text (gap/of input) :point (count input))))

(defn select-candidate
  "Fill the minibuffer with a candidate while keeping the current list."
  [s index]
  (let [{:keys [candidates completer]} (:mini s)]
    (if-let [candidate (get candidates index)]
      (cond-> (-> s (assoc-in [:mini :sel] index) (set-input candidate))
        (map? completer) (update :mini assoc :input candidate :for candidate))
      s)))

;; --- Requests ---

(defonce ^:private req-counter (atom 0))
(defonce ^:private inflight (atom {}))

(defn start!
  "Launch `sources` for `prefix`. Returns the request id."
  [s prefix sources]
  (let [id (swap! req-counter inc)]
    (swap! inflight assoc id {:remaining (count sources) :results []})
    (doseq [src sources]
      (future
        (let [cands (try (vec (src prefix {:state s})) (catch Exception _ []))]
          (swap! inflight
                 (fn [m]
                   (if (contains? m id)
                     (update m id #(-> % (update :results conj cands)
                                      (update :remaining dec)))
                     m))))))
    id))

(defn- finished
  "Candidates of `id` once every source reported, consuming the request."
  [id]
  (when-let [r (get @inflight id)]
    (when (zero? (:remaining r))
      (swap! inflight dissoc id)
      {:candidates (->> (mapcat identity (:results r)) distinct sort (take 500) vec)})))

(defn- sources-of [completer s]
  (if-let [srcs (:sources completer)]
    srcs
    (sources-for s)))

(defn tick!
  "Editor-loop step: start a request when the minibuffer prefix changed, and
   deliver finished candidates. Runs outside `swap!` so futures are launched
   once per keystroke, not once per CAS retry."
  [editor]
  (let [s @editor
        m (:mini s)]
    (when-not (and m (map? (:completer m)))
      ;; A cancelled prompt leaves its in-flight request (and every candidate
      ;; it collected) behind forever.
      (when (seq @inflight) (reset! inflight {})))
    (when (and m (map? (:completer m)))
      (let [prefix (or (:input m) "")]
        (if (not= prefix (:for m))
          (do
            ;; The previous prefix's request can no longer be shown.
            (swap! inflight dissoc (:request m))
            (let [state (assoc s :buf (:prev-buf m (:buf s)))
                  id (start! state prefix (sources-of (:completer m) state))
                  updated (swap! editor
                                 (fn [current]
                                   (if (= m (:mini current))
                                     (update current :mini assoc :for prefix :request id :sel -1)
                                     current)))]
              (when (not= id (get-in updated [:mini :request]))
                (swap! inflight dissoc id))))
          (when-let [r (and (:request m) (finished (:request m)))]
            (swap! editor
                   (fn [current]
                     (let [mini (:mini current)]
                       (if (and (= (:request m) (:request mini)) (= prefix (:for mini))
                                (= (:input m) (:input mini)))
                         (let [candidates (:candidates r)]
                           (cond-> (update current :mini
                                           #(-> % (assoc :candidates candidates :request nil)
                                                  (dissoc :tab-pending?)))
                             (and (:tab-pending? mini) (= 1 (count candidates)))
                             (select-candidate 0)))
                         current))))))))))
