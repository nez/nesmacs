(ns nesmacs.packages
  "Catalog, on-disk package state and startup loader for Clojure extensions."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [nesmacs.api :as api]
            [nesmacs.log :as log]
            [nesmacs.package-archive :as archive]
            [nesmacs.persist :as persist])
  (:import [clojure.lang Compiler DynamicClassLoader RT]
           [java.io FileOutputStream PushbackReader StringReader]
           [java.net HttpURLConnection URI]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files OpenOption StandardCopyOption]
           [java.util.jar JarFile]))

(def ^:private store-name "packages")
(def ^:private format-version 1)
(def ^:private extension-apis #{1 2})
(def ^:private catalog-limit (* 1024 1024))
(def ^:private manifest-limit (* 64 1024))
(def ^:private artifact-limit (* 32 1024 1024))
(def ^:private store-lock (Object.))
(def empty-store {:format format-version :catalog [] :installed {}})

(defn- fail [message]
  (throw (ex-info message {})))

(defn- digest? [value]
  (and (string? value) (re-matches #"[a-f0-9]{64}" value)))

(def ^:private bundled
  (delay
    (into {}
          (keep (fn [directory]
                  (try
                    (let [{:keys [release] :as package} (archive/build directory)]
                      [(:id release) (assoc package :release
                                            (assoc release :url (str "bundled:" directory)
                                                           :bundled? true))])
                    (catch Exception e
                      (log/log "packages: could not build bundled package:" directory (.getMessage e))
                      nil))))
          ["forge" "http"])))

(defn- bundled-packages [] @bundled)

(defn catalog-entries
  "The selected catalog takes precedence over the bundled releases."
  [store]
  (vals (into (update-vals (bundled-packages) :release)
              (map (juxt :id identity))
              (:catalog store))))

(defn- valid-id? [id]
  (and (keyword? id) (namespace id)
       (every? #(re-matches #"[a-z0-9][a-z0-9._-]*" %) [(namespace id) (name id)])))

(defn- short-text? [value limit]
  (and (string? value) (<= 1 (count value) limit)
       (not (str/blank? value)) (not (re-find #"\p{Cntrl}" value))))

(defn compatible? [release]
  (contains? extension-apis (:api release)))

(defn update-available? [record release]
  (and record (compatible? release)
       (not= (:sha256 release) (get-in record [:release :sha256]))
       (or (not (:bundled? release)) (get-in record [:release :bundled?]))))

(defn- https? [^URI uri] (= "https" (.getScheme uri)))

(defn- remote? [^URI uri]
  (and (some? (.getHost uri)) (nil? (.getUserInfo uri)) (nil? (.getFragment uri))
       (or (https? uri)
           (and (= "http" (.getScheme uri))
                (#{"localhost" "127.0.0.1" "[::1]"} (.getHost uri))))))

(defn- local? [^URI uri]
  (and (= "file" (.getScheme uri)) (nil? (.getAuthority uri))
       (nil? (.getQuery uri)) (nil? (.getFragment uri))))

(defn- source-uri ^URI [source]
  (let [s (str/trim source)
        uri (if (re-find #"^[a-zA-Z][a-zA-Z0-9+.-]*:" s)
              (URI/create s)
              (-> (io/file (if (str/starts-with? s "~/")
                             (str (System/getProperty "user.home") (subs s 1))
                             s))
                  .getCanonicalFile .toURI))]
    (when-not (or (remote? uri) (local? uri))
      (fail "Use an HTTPS catalog URL or a local catalog file"))
    uri))

(defn- read-bytes [^java.io.InputStream input limit]
  (let [bytes (.readNBytes input (inc limit))]
    (when (> (alength bytes) limit) (fail "Download exceeds the size limit"))
    bytes))

(defn- fetch [^URI uri limit]
  (loop [url uri redirects 0]
    (if (local? url)
      (with-open [input (io/input-stream (.toURL url))]
        [(read-bytes input limit) url])
      (let [^HttpURLConnection conn (.openConnection (.toURL url))
            [result target]
            (try
              (.setInstanceFollowRedirects conn false)
              (.setConnectTimeout conn 6000)
              (.setReadTimeout conn 6000)
              (let [status (.getResponseCode conn)]
                (cond
                  (= status 200) (with-open [input (.getInputStream conn)]
                                   [(read-bytes input limit) url])
                  (#{301 302 303 307 308} status)
                  (let [location (.getHeaderField conn "Location")
                        target (when location (.resolve url location))]
                    (when-not (and (< redirects 5) target (remote? target)
                                   (or (not (https? url)) (https? target)))
                      (fail "Invalid catalog or package redirect"))
                    [nil target])
                  :else (fail (str "HTTP " status " from " (.getHost url)))))
              (finally (.disconnect conn)))]
        (if result [result target] (recur target (inc redirects)))))))

(defn- read-document [bytes]
  (try
    (with-open [reader (PushbackReader. (StringReader. (String. bytes StandardCharsets/UTF_8)))]
      (let [data (edn/read {:eof ::eof} reader)]
        (when-not (and (map? data) (= ::eof (edn/read {:eof ::eof} reader)))
          (fail "Expected a single EDN map"))
        data))
    (catch StackOverflowError _
      (fail "Package data is nested too deeply"))))

(defn- resolve-artifact [^URI source url]
  (let [target (.resolve source ^String url)]
    (when-not (if (local? source)
                (or (local? target) (remote? target))
                (and (remote? target) (or (not (https? source)) (https? target))))
      (fail "Invalid package download URL"))
    (str target)))

(defn- valid-release? [{:keys [id name description version api url sha256]}]
  (and (valid-id? id)
       (short-text? name 80)
       (short-text? description 300)
       (short-text? version 64)
       (re-matches #"[a-zA-Z0-9][a-zA-Z0-9._+-]*" version)
       (pos-int? api)
       (short-text? url 4096)
       (digest? sha256)))

(defn- unique-by? [f entries]
  (= (count entries) (count (into #{} (map f) entries))))

(defn- read-catalog [source]
  (let [[bytes base] (fetch (source-uri source) catalog-limit)
        {:keys [format packages]} (read-document bytes)]
    (when-not (and (= format-version format) (vector? packages)
                   (every? #(and (map? %) (valid-release? %)) packages)
                   (unique-by? :id packages))
      (fail "Invalid extension catalog"))
    (mapv (fn [release]
            (-> (select-keys release [:id :name :description :version :api :url :sha256])
                (update :url #(resolve-artifact base %))))
          packages)))

(defn valid-option? [{:keys [type choices]} value]
  (case type
    :boolean (boolean? value)
    :string (and (string? value) (<= (count value) 4096))
    :choice (some #{value} choices)
    false))

(defn- option-spec? [{:keys [key label type choices default] :as option}]
  (and (map? option) (keyword? key) (short-text? label 80)
       (or (not= type :choice)
           (and (vector? choices) (seq choices) (every? #(short-text? % 80) choices)
                (apply distinct? choices)))
       (valid-option? option default)))

(defn- artifact-file ^java.io.File [sha]
  (when-not (digest? sha) (fail "Invalid package checksum"))
  (io/file (persist/directory) "plugins" (str sha ".jar")))

(defn- verify [bytes checksum]
  (when-not (= checksum (archive/sha256 bytes))
    (fail "Package checksum does not match the catalog"))
  bytes)

(defn- read-manifest [file {:keys [id version api]}]
  (with-open [archive (JarFile. ^java.io.File file)]
    (let [manifest (when-let [entry (.getJarEntry archive "plugin.edn")]
                     (with-open [input (.getInputStream archive entry)]
                       (read-document (read-bytes input manifest-limit))))
          entry (:entry manifest)
          options (:options manifest [])]
      (when-not (and (= id (:id manifest)) (= version (:version manifest))
                     (= api (:api manifest)) (symbol? entry) (namespace entry)
                     (vector? options) (every? option-spec? options)
                     (unique-by? :key options))
        (fail "Package manifest is missing, invalid or different from the catalog"))
      {:entry entry :options options})))

(defn- release-bytes [{:keys [id url bundled?]}]
  (if bundled?
    (or (get-in (bundled-packages) [id :bytes]) (fail "Bundled package is missing"))
    (first (fetch (URI/create url) artifact-limit))))

(defn- download! [{:keys [sha256] :as release}]
  (let [file (artifact-file sha256)
        dir (.getParentFile file)]
    (io/make-parents file)
    (if (and (.isFile file)
             (with-open [input (io/input-stream file)]
               (= sha256 (archive/sha256 (read-bytes input artifact-limit)))))
      (read-manifest file release)
      (let [bytes (release-bytes release)
            tmp (Files/createTempFile (.toPath dir) ".package-" ".jar"
                                      (make-array java.nio.file.attribute.FileAttribute 0))]
        (try
          (Files/write tmp (verify bytes sha256) (make-array OpenOption 0))
          (let [manifest (read-manifest (.toFile tmp) release)]
            (Files/move tmp (.toPath file) (into-array [StandardCopyOption/ATOMIC_MOVE
                                                       StandardCopyOption/REPLACE_EXISTING]))
            manifest)
          (finally (Files/deleteIfExists tmp)))))))

(defn- options-for [manifest values]
  (into {} (map (fn [{:keys [key default] :as option}]
                  (let [value (get values key)]
                    [key (if (valid-option? option value) value default)]))
                (:options manifest))))

(defn- read-store []
  (let [saved (persist/load! store-name)
        store (if (and (= format-version (:format saved)) (map? (:installed saved))
                       (vector? (:catalog saved)))
                saved empty-store)
        omitted (:removed-bundled store #{})]
    (reduce-kv (fn [s id {:keys [release manifest]}]
                 (if (or (contains? (:installed s) id) (contains? omitted id))
                   s
                   (assoc-in s [:installed id]
                             {:release release :manifest (select-keys manifest [:entry :options])
                              :options (options-for manifest {}) :enabled? true :confirmed? true})))
               store (bundled-packages))))

(defn- with-store! [f]
  ;; The editor loop serializes requests within one instance. The file lock
  ;; also keeps two editor processes from overwriting each other's changes.
  (locking store-lock
    (let [file (io/file (persist/directory) "packages.lock")]
      (io/make-parents file)
      (with-open [output (FileOutputStream. file true)
                  _lock (.lock (.getChannel output))]
        (let [before (read-store)
              {:keys [store] :as result} (f before)]
          (when-not (= before store) (persist/save! store-name store))
          result)))))

(defn- install-release [store {:keys [id sha256] :as release}]
  (let [old (get-in store [:installed id])]
    (if (= sha256 (get-in old [:release :sha256]))
      store
      (let [manifest (download! release)
            previous (if (:confirmed? old) (dissoc old :previous) (:previous old))]
        (-> store
            (assoc-in [:installed id]
                      {:release release :manifest manifest :enabled? (:enabled? old true)
                       :options (options-for manifest (:options old)) :previous previous})
            (update :removed-bundled disj id))))))

(defn- install [store {:keys [id checksum]}]
  (let [release (some #(when (= id (:id %)) %) (catalog-entries store))]
    (when-not (= checksum (:sha256 release))
      (fail "The catalog changed. Refresh the hub and choose the package again"))
    (when-not (compatible? release) (fail "Package needs a different extension API"))
    (install-release store release)))

(defn- catalog-data [source]
  (let [uri (str (source-uri source))]
    {:source uri :catalog (read-catalog uri)}))

(defn- stage-update [{:keys [store] :as result} {:keys [id] :as release}]
  (try
    (-> result
        (assoc :store (install-release store release))
        (update :updated conj id))
    (catch Exception e
      (log/log "packages: could not update" id (.getMessage e))
      (assoc-in result [:failed id] (.getMessage e)))))

(defn update-all!
  "Refresh the selected catalog and stage updates for installed packages."
  [default-source]
  (with-store!
    (fn [store]
      (let [source (or (:source store) default-source)
            {:keys [data error]} (when source
                                  (try {:data (catalog-data source)}
                                       (catch Exception e
                                         (log/log "packages: could not refresh catalog:" (.getMessage e))
                                         {:error (.getMessage e)})))
            store (merge store data)
            upgrades (->> (catalog-entries store)
                          (filter #(update-available? (get-in store [:installed (:id %)]) %))
                          (sort-by (comp str :id)))]
        (reduce stage-update {:store store :updated [] :failed {} :catalog-error error}
                upgrades)))))

(defn- select-option [record key value]
  (let [option (some #(when (= key (:key %)) %) (get-in record [:manifest :options]))]
    (when-not (and option (valid-option? option value))
      (fail "Invalid package option"))
    (assoc-in record [:options key] value)))

(defn- change-installed [store {:keys [action id option value]}]
  (let [record (get-in store [:installed id])]
    (when-not record (fail "Package is no longer installed"))
    (case action
      :enable (assoc-in store [:installed id :enabled?] true)
      :disable (assoc-in store [:installed id :enabled?] false)
      :remove (cond-> (update store :installed dissoc id)
                (contains? (bundled-packages) id)
                (update :removed-bundled (fnil conj #{}) id))
      :configure (assoc-in store [:installed id] (select-option record option value)))))

(defn execute!
  "Run one hub operation off the editor thread and return the committed store."
  [{:keys [action source] :as request}]
  (let [data (when (= :catalog action) (catalog-data source))]
    (:store (with-store! (fn [store]
                           {:store (case action
                                     :catalog (merge store data)
                                     :install (install store request)
                                     (change-installed store request))})))))

(defn- load-extension [s {:keys [release options]}]
  (let [file (artifact-file (:sha256 release))]
    (when-not (compatible? release) (fail "Package needs a different extension API"))
    (when (:bundled? release) (download! release))
    (with-open [input (io/input-stream file)]
      (verify (read-bytes input artifact-limit) (:sha256 release)))
    (let [{:keys [entry] :as manifest} (read-manifest file release)
          loader (DynamicClassLoader. (RT/baseLoader))
          namespace (symbol (namespace entry))
          staged (atom s)]
      (try
        (.addURL loader (-> file .toURI .toURL))
        (binding [api/*state* staged]
          (with-bindings {Compiler/LOADER loader}
            (when (find-ns namespace) (fail (str "Package namespace is already loaded: " namespace)))
            (require namespace)
            (let [start (ns-resolve namespace (symbol (name entry)))]
              (when-not (and start (ifn? @start)) (fail "Package entry function is missing"))
              (start (options-for manifest options)))))
        @staged
        (catch Throwable e
          (.close loader)
          (throw e))))))

(defn- record-if-current! [id before after]
  (try
    (= after (get-in (with-store! (fn [store]
                                  {:store (cond-> store
                                            (= before (get-in store [:installed id]))
                                            (assoc-in [:installed id] after))}))
                     [:store :installed id]))
    (catch Exception e
      (log/log "packages: could not save startup result:" (.getMessage e))
      false)))

(defn- start-extension [s [id {:keys [release enabled? previous] :as record}]]
  (if-not enabled?
    s
    (let [result (try {:state (load-extension s record)}
                      (catch Exception e {:error e})
                      (catch AssertionError e {:error e})
                      (catch LinkageError e {:error e}))]
      (if-let [error (:error result)]
        (let [restored? (and previous (record-if-current! id record (assoc previous :enabled? enabled?)))
              message (str (.getMessage ^Throwable error)
                           (when restored? " | Previous version selected. Restart to load it."))]
          (log/log "packages:" id message)
          (assoc-in s [:packages :errors id] message))
        (do
          (when-not (:confirmed? record)
            (record-if-current! id record (-> record (assoc :confirmed? true) (dissoc :previous))))
          (assoc-in (:state result) [:packages :active id]
                    {:release release :options (:options record)}))))))

(defn initialize!
  "Load installed packages at startup, each against a private candidate state."
  [s]
  (let [store (read-store)
        loaded (reduce start-extension s (sort-by (comp str key) (:installed store)))]
    (assoc-in loaded [:packages :store] (read-store))))
