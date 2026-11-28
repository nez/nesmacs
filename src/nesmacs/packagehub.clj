(ns nesmacs.packagehub
  "Keyboard-first extension manager shared by the terminal and web clients."
  (:require [clojure.string :as str]
            [nesmacs.cmd :as cmd]
            [nesmacs.complete :as complete]
            [nesmacs.listbuf :as listbuf]
            [nesmacs.log :as log]
            [nesmacs.packages :as packages]
            [nesmacs.props :as props]))

(def ^:private buffer-name "*Extensions*")

(defn- source [s]
  (or (get-in s [:packages :store :source]) (System/getenv "NESMACS_EXTENSION_CATALOG")))

(defn- rows [{:keys [store active errors update-errors]}]
  (let [catalog (into {} (map (juxt :id identity)) (packages/catalog-entries store))
        installed (:installed store)]
    (for [id (into (set (keys catalog)) (concat (keys installed) (keys active)))
          :let [available (catalog id)
                record (get installed id)
                running (get active id)
                release (or (:release record) available (:release running))]]
      {:id id :release release :available available :record record
       :active running :error (get errors id) :update-error (get update-errors id)})))

(defn- update-available? [{:keys [record available]}]
  (packages/update-available? record available))

(defn- status [{:keys [record active release error] :as row}]
  (cond
    (and (nil? record) active) "Removal pending restart"
    (and record (not (:enabled? record))) (if active "Disable pending restart" "Disabled")
    error "Load failed"
    (nil? record) (if (packages/compatible? release) "Available" "Incompatible")
    (nil? active) "Restart to activate"
    (not= (:sha256 release) (get-in active [:release :sha256])) "Restart to update"
    (not= (:options record) (:options active)) "Restart to apply options"
    (update-available? row) "Update available"
    :else "Active"))

(defn- package-actions [{:keys [record available active] :as row}]
  (cond-> []
    (and (nil? record) available (packages/compatible? available))
    (conj {:action :install :label (str "Install " (:version available))})
    (update-available? row)
    (conj {:action :install :label (str "Update to " (:version available))})
    (seq (get-in record [:manifest :options]))
    (conj {:action :configure :label "Configure options"})
    record
    (conj (if (:enabled? record)
            {:action :disable :label "Disable"}
            {:action :enable :label "Enable"})
          {:action :remove :label "Uninstall"})
    (and active (nil? record))
    (conj {:message "Restart Nesmacs to finish uninstalling."})))

(defn- detail-spec [{:keys [release error update-error] :as row}]
  {:header (str (:name release) "  " (:version release))
   :entries (into (package-actions row)
                  (cond-> [{:message (status row)}
                           {:message (:description release)}
                           {:message (if (:bundled? release)
                                       "Source: Included with Nesmacs"
                                       (str "Source: " (:url release)))}]
                    error (conj {:message (str "Error: " error)})
                    update-error (conj {:message (str "Update failed: " update-error)})))})

(defn- matches? [query {:keys [id release]}]
  (let [text (str/lower-case (str id " " (:name release) " " (:description release)))]
    (every? #(str/includes? text %) (str/split (str/lower-case (or query "")) #"\s+"))))

(defn- list-spec [s all-rows]
  (let [{:keys [view query operation job]} (:packages s)
        installed? (= view :installed)
        visible (->> all-rows
                     (filter #(or (not installed?) (:record %) (:active %)))
                     (filter #(matches? query %))
                     (sort-by #(get-in % [:release :name]))
                     vec)]
    {:header (str "Extensions  "
                  (if installed? "Explore  [Installed]" "[Explore]  Installed")
                  (when-not (str/blank? query) (str "  / " query)))
     :entries (if (seq visible)
                visible
                [{:message (cond
                             (or operation job) "Working…"
                             (not (str/blank? query)) "No matching packages. Press / to clear the search."
                             installed? "No packages installed. Press TAB to explore."
                             (source s) "Catalog is empty. Press g to refresh."
                             :else "Press s to set a catalog URL or local file.")}])}))

(defn- format-entry [{:keys [release label message] :as row}]
  (str "  " (or label message
               (str (:name release) "  " (:version release) "  [" (status row) "]  "
                    (:description release)))))

(defn- detail-row [s all-rows]
  (let [id (get-in s [:packages :detail])]
    (some #(when (= id (:id %)) %) all-rows)))

(defn- view-spec [s]
  (let [all (rows (:packages s))
        spec (if-let [detail (detail-row s all)] (detail-spec detail) (list-spec s all))]
    (assoc spec :name buffer-name :mode :packagehub-mode :format-entry format-entry
           :extras {:highlight-line? true
                    :props [(props/make 0 (count (:header spec)) :prompt)]})))

(defn- refresh-view [s]
  (if (get-in s [:bufs buffer-name])
    (listbuf/refresh s buffer-name (view-spec s))
    s))

(defn- first-entry [s]
  (let [text (str (:text (cmd/cur s)))
        newline (str/index-of text "\n")]
    (if newline (cmd/set-point s (constantly (inc newline))) s)))

(defn- queue [s request]
  (if (or (get-in s [:packages :operation]) (get-in s [:packages :job]))
    (cmd/msg s "Package operation in progress")
    (-> s
        (assoc-in [:packages :operation] request)
        refresh-view
        (cmd/msg "Working on extensions…"))))

(defn refresh-catalog [s]
  (if-let [url (source s)]
    (queue s {:action :catalog :source url})
    (cmd/msg s "Set a catalog with s or M-x package-catalog")))

(defn update-all [s]
  (queue s {:action :update-all :source (source s)}))

(defn open [s]
  (let [already-open? (= buffer-name (:buf s))
        first-open? (not (get-in s [:packages :opened?]))
        s (cond-> s
            (not already-open?) (assoc-in [:packages :return-to] (:buf s))
            first-open? (assoc-in [:packages :opened?] true))]
    (cond-> (-> s
                (listbuf/refresh buffer-name (view-spec s))
                (cmd/set-cur-buffer buffer-name)
                first-entry)
      (and first-open? (source s)) refresh-catalog)))

(defn restore [s]
  (refresh-view s))

(defn- show-list [s]
  (-> s (update :packages dissoc :detail) refresh-view first-entry))

(defn switch-view [s]
  (-> s
      (update-in [:packages :view] #(if (= % :installed) :explore :installed))
      show-list))

(defn back [s]
  (if (get-in s [:packages :detail])
    (show-list s)
    (cmd/remove-buffer s buffer-name (get-in s [:packages :return-to]))))

(defn search [s mini-start]
  (mini-start s "Search packages (blank clears): "
              (fn [st input]
                (-> st
                    (assoc-in [:packages :query] (str/trim input))
                    show-list))))

(defn set-source [s mini-start]
  (mini-start s "Catalog URL or file: "
              (fn [st input]
                (if (str/blank? input)
                  (cmd/msg st "Catalog unchanged")
                  (queue st {:action :catalog :source (str/trim input)})))))

(defn- choose [s mini-start prompt choices on-done]
  (mini-start s prompt
              (fn [st input]
                (if-let [choice (some #{input} choices)]
                  (on-done st choice)
                  (cmd/msg st "Choose one of the listed values")))
              (fn [input _]
                (let [matches (filterv #(str/starts-with? % input) choices)]
                  {:candidates matches
                   :completed (if (= 1 (count matches)) (first matches) input)}))))

(defn- option-value [s mini-start id {:keys [key label type choices default]} record]
  (let [request {:action :configure :id id :option key}
        current (get (:options record) key default)
        save (fn [st value] (queue st (assoc request :value value)))
        prompt (str label ": ")]
    (case type
      :boolean (choose s mini-start prompt ["true" "false"] #(save %1 (= "true" %2)))
      :choice (choose s mini-start prompt choices save)
      :string (-> (mini-start s prompt save) (complete/set-input current)))))

(defn- configure [s mini-start id record]
  (let [options (get-in record [:manifest :options])
        names (into {} (map #(vector (subs (str (:key %)) 1) %)) options)]
    (if (= 1 (count options))
      (option-value s mini-start id (first options) record)
      (choose s mini-start "Package option: " (sort (keys names))
              (fn [st option] (option-value st mini-start id (names option) record))))))

(defn select-entry [s mini-start]
  (let [{:keys [id action]} (listbuf/entry-at-point s)
        detail (get-in s [:packages :detail])]
    (cond
      id (-> s (assoc-in [:packages :detail] id) refresh-view first-entry)
      action
      (let [{:keys [record available release]} (detail-row s (rows (:packages s)))
            request {:action action :id detail}]
        (case action
          :configure (configure s mini-start detail record)
          :install (queue s (assoc request :checksum (:sha256 available)))
          :remove (choose s mini-start (str "Uninstall " (:name release) "? ") ["yes" "no"]
                          (fn [st answer] (if (= answer "yes") (queue st request) st)))
          (queue s request)))
      :else s)))

(defn- update-message [{:keys [updated failed catalog-error]}]
  (str (if (or (seq updated) (seq failed))
         (str (count updated) " updated, " (count failed) " failed.")
         "No package updates found.")
       (when (seq updated) " Restart to apply.")
       (when catalog-error " Used cached catalog.")
       (when (seq failed) " See package details.")))

(defn- done-message [action result]
  (case action
    :catalog "Catalog refreshed"
    :install "Package saved. Restart Nesmacs to load it."
    :update-all (update-message result)
    :configure "Options saved. Restart Nesmacs to apply them."
    :remove "Package uninstalled. Restart Nesmacs to finish."
    "Package setting saved. Restart Nesmacs to apply it."))

(defn- perform [{:keys [action source] :as request}]
  (try (if (= action :update-all)
         (packages/update-all! source)
         {:store (packages/execute! request)})
       (catch Exception e
         (log/log "packages:" (name action) (.getMessage e))
         {:error (.getMessage e)})))

(defn tick!
  "Run network and disk work off the editor thread; apply results in its loop."
  [editor]
  (let [{:keys [operation job]} (:packages @editor)]
    (cond
      (and job (realized? (:result job)))
      (let [{:keys [store error updated failed] :as result} @(:result job)
            {:keys [action id]} (:request job)]
        (swap! editor
               (fn [s]
                 (-> s
                     (update :packages dissoc :job)
                     (cond-> store (assoc-in [:packages :store] store)
                       (and store id) (update-in [:packages :errors] dissoc id)
                       (and store id) (update-in [:packages :update-errors] dissoc id)
                       (seq updated) (update-in [:packages :errors] #(apply dissoc % updated))
                       failed (assoc-in [:packages :update-errors] failed)
                       (and store (= action :remove)) (update :packages dissoc :detail))
                     refresh-view
                     (cmd/msg (if error (str "Extensions: " error) (done-message action result)))))))
      operation
      (let [result (future (perform operation))]
        (swap! editor update :packages
               #(-> % (dissoc :operation) (assoc :job {:request operation :result result})))))))
