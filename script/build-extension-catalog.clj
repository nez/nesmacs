(require '[clojure.java.io :as io]
         '[clojure.pprint :as pprint]
         '[nesmacs.package-archive :as archive])

(let [output (io/file "target/extension-catalog")
      directories (->> (.listFiles (io/file "extensions"))
                       (filter #(.isFile (io/file % "plugin.edn")))
                       (map #(.getName %))
                       sort)
      packages (mapv (fn [directory]
                       (let [{:keys [filename bytes release]} (archive/build directory)
                             file (io/file output filename)]
                         (io/make-parents file)
                         (with-open [out (io/output-stream file)] (.write out ^bytes bytes))
                         (assoc release :url filename)))
                     directories)
      catalog (io/file output "catalog.edn")]
  (io/make-parents catalog)
  (spit catalog (with-out-str (pprint/pprint {:format 1 :packages packages})))
  (println (.getAbsolutePath catalog)))
