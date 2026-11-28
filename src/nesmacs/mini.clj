(ns nesmacs.mini
  (:require [clojure.string :as str]
            [nesmacs.api :as api]
            [nesmacs.buf :as buf]
            [nesmacs.cmd :as cmd]
            [nesmacs.complete :as complete]))

(def ^:private mini-buf-name " *mini*")

(defn start
  ([s prompt on-done] (start s prompt on-done nil))
  ([s prompt on-done completer]
   (if (:mini s)
     ;; A minibuffer is already active. Opening a second one would drop the
     ;; outer prompt's `prev-buf` / `on-done` and leave the editor wedged.
     (cmd/msg s "Command attempted to use disabled command in minibuffer")
     (-> s
         (assoc-in [:bufs mini-buf-name] (assoc (buf/make mini-buf-name) :journal? false))
         (assoc :mini {:prompt prompt :on-done on-done :prev-buf (:buf s)
                       :history-idx -1 :completer completer})
         (assoc :buf mini-buf-name)))))

(defn close [s]
  (let [[s target] (cmd/ensure-buffer s (:prev-buf (:mini s)))]
    (assoc s :buf target :mini nil)))

(defn accept [s]
  (if-let [{:keys [on-done]} (:mini s)]
    (let [input  (str (:text (cmd/cur s)))
          appended? (and (seq input) (not= input (peek (:mini-history s))))
          s' (-> s
                 close
                 (cond-> appended? (update :mini-history conj input)))]
      (on-done (cond-> s' appended? (api/persist-setting :mini-history)) input))
    s))

(defn- mini-move-candidate
  "Move candidate selection by `delta`, starting at the near end of the list."
  [s delta]
  (let [n (count (get-in s [:mini :candidates]))
        i (get-in s [:mini :sel] -1)
        i (if (and (neg? i) (neg? delta)) n i)
        j (-> (+ i delta) (max 0) (min (dec n)))]
    (complete/select-candidate s j)))

(defn- mini-next-candidate [s] (mini-move-candidate s 1))
(defn- mini-prev-candidate [s] (mini-move-candidate s -1))

(defn- mini-history-move
  "Move minibuffer history selection. dir=+1 goes back (older), -1 forward (newer).
   idx=-1 means cleared (no selection); 0..n-1 picks from newest to oldest."
  [s dir]
  (let [hist (:mini-history s [])
        n    (count hist)
        idx  (get-in s [:mini :history-idx])
        new  (-> (+ idx dir) (max -1) (min (dec n)))]
    (cond
      (= new idx) s
      (neg? new)  (-> s (assoc-in [:mini :history-idx] -1) (complete/set-input ""))
      :else       (-> s (assoc-in [:mini :history-idx] new)
                        (complete/set-input (get hist (- (dec n) new)))))))

(defn- mini-history-prev [s] (mini-history-move s 1))
(defn- mini-history-next [s] (mini-history-move s -1))

(defn- longest-common-prefix [[^String first-match & others]]
  (let [length (reduce (fn [length ^String candidate]
                         (loop [i 0]
                           (if (and (< i length)
                                    (.regionMatches first-match true i candidate i 1))
                             (recur (inc i))
                             i)))
                       (.length first-match) others)]
    (subs first-match 0 length)))

(defn complete-prefix [input candidates]
  (let [matches (filterv #(complete/prefix-match? input %) candidates)]
    {:completed (if (seq matches) (longest-common-prefix matches) input)
     :candidates matches}))

(defn file-completer
  "Tab-complete file paths. Returns {:completed str :candidates [str]}."
  ([input] (file-completer input nil))
  ([input _s]
   (let [f (java.io.File. (if (empty? input) "." input))
         [parent prefix] (if (.isDirectory f)
                           [f ""]
                           [(.getParentFile f) (.getName f)])
         parent (or parent (java.io.File. "."))
         children (try (vec (.list parent)) (catch Exception _ []))
         {:keys [completed candidates]} (complete-prefix prefix children)
         path (fn [name] (.getPath (java.io.File. parent ^String name)))
         as-dir (fn [s] (if (str/ends-with? s "/") s (str s "/")))
         paths (mapv (fn [name]
                       (let [full (path name)]
                         (if (.isDirectory (java.io.File. full)) (as-dir full) full)))
                     candidates)]
     {:completed (case (count paths)
                   0 input
                   1 (first paths)
                   (if (empty? completed) (as-dir (path completed)) (path completed)))
      :candidates paths})))

(defn- mini-tab-complete [s]
  (let [{:keys [completer candidates input for request]} (:mini s)]
    (cond
      (map? completer) (if (and (some? for) (= input for) (nil? request))
                         (if (= 1 (count candidates)) (complete/select-candidate s 0) s)
                         (assoc-in s [:mini :tab-pending?] true))
      completer (let [input (str (:text (cmd/cur s)))
                      {:keys [completed]} (completer input s)]
                  (-> s (complete/set-input (or completed input))
                        (update :mini dissoc :sel)))
      :else s)))

(defn refresh-candidates
  "Run the completer with the current minibuffer text and stash matches on
   :mini. Called after every keystroke so candidates narrow live as the
   user types — no TAB needed. Map completers are asynchronous: they only
   record the input here, and `complete/tick!` fills the candidates."
  [s]
  (let [{:keys [completer candidates sel] :as mini} (:mini s)
        input (str (:text (cmd/cur s)))]
    (cond
      (map? completer) (cond-> (assoc-in s [:mini :input] input)
                         (not= input (:input mini ""))
                         (update :mini dissoc :for :sel :candidates :tab-pending?))
      (and completer (not= input (get candidates sel)))
      (update s :mini #(-> % (assoc :candidates (vec (:candidates (completer input s))))
                             (dissoc :sel)))
      :else s)))

(def keymap
  {:return   accept
   [:meta \p] mini-history-prev
   [:meta \n] mini-history-next
   [:ctrl \n] mini-next-candidate
   [:ctrl \p] mini-prev-candidate
   :tab      mini-tab-complete})
