(ns nesmacs.package-archive
  "Build the same extension JAR from resources in a checkout or release."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io ByteArrayOutputStream]
           [java.security MessageDigest]
           [java.time LocalDateTime]
           [java.util HexFormat]
           [java.util.jar JarEntry JarOutputStream]))

(defn sha256 [bytes]
  (.formatHex (HexFormat/of) (.digest (MessageDigest/getInstance "SHA-256") bytes)))

(defn build [directory]
  (let [resource #(or (io/resource (str directory "/" %))
                      (throw (ex-info (str "Missing extension file: " directory "/" %) {})))
        manifest (edn/read-string (slurp (resource "plugin.edn") :encoding "UTF-8"))
        entry-path (str (-> (:entry manifest) namespace (str/replace "-" "_")
                            (str/replace "." "/")) ".clj")
        paths (sort (distinct (into ["plugin.edn" entry-path] (:files manifest))))
        output (ByteArrayOutputStream.)]
    (with-open [jar (JarOutputStream. output)]
      (doseq [path paths]
        (.putNextEntry jar (doto (JarEntry. path) (.setTimeLocal (LocalDateTime/of 1980 1 1 0 0))))
        (with-open [input (io/input-stream (resource path))]
          (io/copy input jar))
        (.closeEntry jar)))
    (let [bytes (.toByteArray output)]
      {:manifest manifest
       :filename (str directory "-" (:version manifest) ".jar")
       :bytes bytes
       :release (assoc (select-keys manifest [:id :name :description :version :api])
                       :sha256 (sha256 bytes))})))
