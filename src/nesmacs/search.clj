(ns nesmacs.search
  "Interactive search and replace commands."
  (:require [clojure.string :as str]
            [nesmacs.cmd :as cmd]
            [nesmacs.mini :as mini]
            [nesmacs.props :as props]
            [nesmacs.text :as text]))

;; --- Buffer props ---

(defn- set-prop
  "Replace every prop owned by `owner` in the current buffer with one range."
  [s owner face from to]
  (cmd/update-cur s #(assoc % :props (props/set-owner (:props %) owner from to face {}))))

(defn- clear-prop [s owner]
  (cmd/update-cur s #(assoc % :props (props/remove-owner (:props %) owner))))

;; --- Incremental search ---

(defn- isearch-do
  "Search from current point in :isearch's direction. When :extend? is true
   (pattern just grew or shrank), include the current point as a candidate so
   a match anchored there can grow/shrink; otherwise advance past the current
   match so C-s/C-r finds the next occurrence."
  [s]
  (let [{:keys [pattern direction extend?]} (:isearch s)
        b (cmd/cur s) t (:text b) p (:point b)
        tn (count t) pn (count pattern)]
    (if (empty? pattern)
      (clear-prop s :isearch)
      (let [found (if (= direction :forward)
                    (text/search-forward  t pattern (if extend? p (min (+ p pn) tn)))
                    ;; Extend: allow the match starting at p (ending at p+pn)
                    ;; to stay anchored. (inc p) only worked for pn=1 — multi-
                    ;; char patterns were yanked away from the current match.
                    (text/search-backward t pattern (if extend? (+ p pn) p)))]
        (if found
          (-> (cmd/set-point s (fn [_ _] found))
              (set-prop :isearch :match found (+ found pn)))
          (cmd/msg s (str "Failing I-search: " pattern)))))))

(defn isearch-start [s direction]
  (if (:mini s)
    ;; Isearch and the minibuffer can't sensibly share an editor state —
    ;; isearch would operate on the minibuffer text, and the minibuffer's
    ;; prompt would remain live underneath. Refuse.
    (cmd/msg s "Command attempted to use disabled command in minibuffer")
    (-> s (assoc :isearch {:pattern "" :direction direction
                           :origin (:point (cmd/cur s))}))))

(defn- isearch-accept [s]
  ;; Clear :msg too — otherwise a "Failing I-search: foo" from the final
  ;; keystroke survives into the normal echo area after RET.
  (-> s (clear-prop :isearch) (dissoc :isearch) (assoc :msg nil)))

(defn isearch-cancel [s]
  (let [origin (get-in s [:isearch :origin])]
    (-> s (clear-prop :isearch)
          (dissoc :isearch)
          (cmd/set-point (fn [_ _] origin))
          (cmd/msg "Quit"))))

(defn- isearch-edit
  "Update the pattern via `f`, then re-search from the current match."
  [s f]
  (-> s (update :isearch assoc :extend? true)
        (update-in [:isearch :pattern] f)
        isearch-do))

(defn isearch-append [s ch]
  (isearch-edit s #(str % ch)))

(defn- isearch-next [s direction]
  (-> s (update :isearch assoc :direction direction :extend? false)
        isearch-do))

(def ^:private isearch-keys
  {[:ctrl \s]  #(isearch-next % :forward)
   [:ctrl \r]  #(isearch-next % :backward)
   :return    isearch-accept
   [:ctrl \g]  isearch-cancel})

(defn- isearch-backspace [s]
  (if (seq (get-in s [:isearch :pattern]))
    (isearch-edit s #(subs % 0 (dec (count %))))
    (isearch-cancel s)))

(defn handle-isearch
  "Handle a key during incremental search. Returns updated state."
  [s key dispatch]
  (cond
    (isearch-keys key)   ((isearch-keys key) s)
    (= key :backspace)   (isearch-backspace s)
    (and (char? key) (>= (int key) 32)) (isearch-append s (str key))
    (string? key)        (isearch-append s key)
    :else                (-> (isearch-accept s) (dispatch key))))

;; --- Query replace (M-%) ---

(defn- regex-find
  "Return [start end groups] for the next match of `pattern` at or after `from`, or nil."
  [^CharSequence t ^java.util.regex.Pattern pattern ^long from]
  (let [m (re-matcher pattern t)]
    (when (.find m (int from))
      [(.start m) (.end m)
       (vec (for [i (range (inc (.groupCount m)))]
              (or (.group m (int i)) "")))])))

(defn- expand-replacement
  "Replace \\N in `template` with the corresponding match group."
  [^String template groups]
  (str/replace template #"\\(\d)"
               (fn [[_ d]] (or (get groups (Integer/parseInt d)) ""))))

(defn- qr-msg [s]
  (let [{:keys [from to regex?]} (:query-replace s)]
    (cmd/msg s (str (if regex? "Query replacing regexp " "Query replacing ")
                from " with " to ": (y/n/!/q)"))))

(defn- qr-exit [s]
  (let [{:keys [buffer] n :count :or {n 0}} (:query-replace s)
        buffer (or buffer (:buf s))]
    (-> (cond-> s (cmd/buf s buffer)
          (update-in [:bufs buffer :props] props/remove-owner :qr))
          (dissoc :query-replace)
          (cmd/msg (str "Replaced " n " occurrence(s)")))))

(defn- qr-find-from
  "Return [start end groups] for the first match at or after `pos`, or nil."
  [s ^long pos]
  (let [{:keys [from pattern]} (:query-replace s)
        t (:text (cmd/cur s))]
    (when (<= pos (count t))
      (if pattern
        (regex-find t pattern pos)
        (when-let [start (text/search-forward t from pos)]
          [start (+ start (count from)) nil])))))

(defn- qr-show
  "Move point to the match `found` ([start end groups]), or exit when nil."
  [s found]
  (if-let [[start end groups] found]
    (-> s (cmd/goto-char start)
          (set-prop :qr :match start end)
          (update :query-replace assoc :match-start start :match-end end :groups groups
                  :version (:version (cmd/cur s)))
          qr-msg)
    (qr-exit s)))

(defn- qr-advance
  "Skip the current match and show the next one. Search starts past the
   current match's end (past a zero-width match) so `n` always progresses."
  [s]
  (let [{:keys [match-end]} (:query-replace s)
        p (:point (cmd/cur s))]
    (qr-show s (qr-find-from s (max (inc (long p)) (long (or match-end 0)))))))

(defn- qr-replace-here [s]
  (let [{:keys [to regex? match-end groups]} (:query-replace s)
        p (long (:point (cmd/cur s)))
        replacement (if regex? (expand-replacement to groups) to)
        zero-width? (= p (long match-end))
        s2 (-> s (cmd/edit p match-end replacement)
                 (update-in [:query-replace :count] (fnil inc 0)))]
    (if zero-width?
      ;; A zero-width match (e.g. regex `$` or `a*`) would be refound at the
      ;; same spot. Step past it — and exit at EOF — so neither the search
      ;; nor replace-all can loop forever.
      (let [s3 (cmd/update-cur s2 #(update % :point inc))
            b  (cmd/cur s3)]
        (if (>= (long (:point b)) (count (:text b)))
          (qr-exit s2)
          (qr-show s3 (qr-find-from s3 (:point b)))))
      (qr-show s2 (qr-find-from s2 (:point (cmd/cur s2)))))))

(defn- qr-replace-all [s]
  (loop [s (qr-replace-here s)]
    (if (:query-replace s) (recur (qr-replace-here s)) s)))

(defn- query-replace-begin
  "Enter query-replace mode at the first match from point."
  [s from to regex?]
  (try
    (let [pattern (when regex? (re-pattern from))
          s (assoc s :query-replace {:from from :to to :count 0 :regex? regex? :pattern pattern
                                    :buffer (:buf s)})]
      (if-let [found (qr-find-from s (:point (cmd/cur s)))]
        (qr-show s found)
        (cmd/msg (dissoc s :query-replace) "No match")))
    (catch java.util.regex.PatternSyntaxException e
      (cmd/msg s (str "Invalid regexp: " (.getDescription e))))))

(defn- qr-prompt-to
  "Having read FROM, prompt for TO and kick off the interactive loop."
  [s input regex?]
  (if (str/blank? input)
    s
    (mini/start s (str "Query replace " (when regex? "regexp ") input " with: ")
      (fn [s to] (query-replace-begin s input to regex?)))))

(defn query-replace        [s input] (qr-prompt-to s input false))
(defn query-replace-regexp [s input] (qr-prompt-to s input true))

(defn query-replace-cmd [s]
  (if (:read-only (cmd/cur s))
    (cmd/msg s "Buffer is read-only")
    (mini/start s "Query replace: " query-replace)))

(defn query-replace-regexp-cmd [s]
  (if (:read-only (cmd/cur s))
    (cmd/msg s "Buffer is read-only")
    (mini/start s "Query replace regexp: " query-replace-regexp)))

(def ^:private qr-keys
  {\y qr-replace-here
   \n qr-advance
   \! qr-replace-all
   \q qr-exit
   :return qr-exit
   :escape qr-exit})

(defn handle-replace [s key]
  (let [{:keys [buffer version match-start]} (:query-replace s)
        b (cmd/cur s)]
    (if (and (= buffer (:buf s)) (= version (:version b)) (= match-start (:point b))
             (not (:read-only b)))
      ((get qr-keys key qr-exit) s)
      (cmd/msg (qr-exit s) "Replace canceled: buffer or match changed"))))
