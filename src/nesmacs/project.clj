(ns nesmacs.project
  "Project root detection and file discovery.

   The .gitignore matcher here is deliberately minimal: names and `*.ext`
   patterns, accumulated from the root down. No `!` negation, no `**`, no
   path patterns. shortcut: upgrade to a real matcher when a repo in real use
   needs those."
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.io File]
           [java.nio.file Files]
           [java.util Locale]))

(def ^:private markers [".nesmacs.edn" ".git" "deps.edn" "project.clj" ".project"
                        "pyproject.toml" "setup.py" "package.json"])

(defn root
  "Nearest ancestor of `start` (file or directory) holding a project marker.
   Returns an absolute path, or nil."
  [start]
  (loop [f (.getAbsoluteFile (File. (str start)))]
    (when f
      (if (some #(.exists (File. f ^String %)) markers)
        (.getCanonicalPath f)
        (recur (.getParentFile f))))))

(defn root-for
  "Project root of a file path, or of the process cwd when `file` is nil."
  [file]
  (root (or file (System/getProperty "user.dir"))))

(defn context [s]
  (let [key (or (get-in s [:mini :prev-buf]) (:buf s))
        {:keys [project-root file nav-root git-dir job-dir]} (get-in s [:bufs key])]
    (or project-root (some-> file root) nav-root git-dir (some-> job-dir root)
        (some-> (get-in s [:bufs key :terminal :dir]) root)
        (:project-root s) (root-for nil))))

(defn profile [root]
  (let [f (File. ^String root ".nesmacs.edn")]
    (when (.isFile f)
      (let [data (edn/read-string (slurp f :encoding "UTF-8"))]
        (when-not (map? data) (throw (ex-info "Expected an EDN map in .nesmacs.edn" {:file f})))
        data))))

(defn argv? [argv]
  (and (vector? argv) (seq argv) (every? string? argv) (not (str/blank? (first argv)))))

(defn- ignore-patterns [^File dir]
  (let [f (File. dir ".gitignore")]
    (when (.exists f)
      (->> (str/split-lines (slurp f))
           (map str/trim)
           (remove #(or (empty? %) (str/starts-with? % "#")
                        (str/starts-with? % "!")))
           (map #(str/replace % #"^/" ""))
           (remove #(str/includes? % "/"))))))

(defn- ignored? [patterns ^String name]
  (boolean
    (some (fn [p]
            (if (str/starts-with? p "*.")
              (str/ends-with? name (subs p 1))
              (= name p)))
          patterns)))

(defn files
  "Relative paths of every file under `root`, sorted, skipping `.git`,
   symlinked directories and names matched by the minimal ignore matcher."
  [root]
  (let [base (File. (str root))]
    (letfn [(walk [^File dir rel pats]
              (mapcat
                (fn [^File f]
                  (let [n  (.getName f)
                        r  (if (str/blank? rel) n (str rel "/" n))
                        p? (or (= n ".git") (ignored? pats n))]
                    (cond
                      p? []
                      (.isDirectory f)
                      (if (Files/isSymbolicLink (.toPath f))
                        []
                        (walk f r (concat pats (ignore-patterns f))))
                      :else [r])))
                (or (.listFiles dir) [])))]
      (sort (walk base "" (ignore-patterns base))))))

(defn- subsequence? [^String needle ^String hay]
  (loop [i 0 j 0]
    (cond
      (>= i (.length needle)) true
      (>= j (.length hay))    false
      (= (.charAt needle i) (.charAt hay j)) (recur (inc i) (inc j))
      :else                   (recur i (inc j)))))

(defn fuzzy
  "Files matching `input` as a case-insensitive subsequence, best first.
   Exact basename matches rank above earlier and shorter paths."
  [files input]
  (let [q (.toLowerCase ^String (str/trim input) Locale/ROOT)]
    (if (empty? q)
      (vec (take 300 files))
      (->> files
           (filter #(subsequence? q (.toLowerCase ^String % Locale/ROOT)))
           (sort-by (fn [^String f]
                      (let [lf (.toLowerCase f Locale/ROOT)
                            lb (subs lf (inc (.lastIndexOf lf "/")))]
                        [(if (= q lb) 0 1) (or (str/index-of lf q) 999) (count f)])))
           (take 300)
           vec))))
