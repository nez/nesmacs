(ns xmas.project
  "Project root detection and file discovery.

   The .gitignore matcher here is deliberately minimal: names and `*.ext`
   patterns, accumulated from the root down. No `!` negation, no `**`, no
   path patterns. shortcut: upgrade to a real matcher when a repo in real use
   needs those."
  (:require [clojure.string :as str])
  (:import [java.io File]
           [java.nio.file Files]))

(def ^:private markers [".git" "deps.edn" "project.clj" ".project"])

(defn root
  "Nearest ancestor of `start` (file or directory) holding a project marker.
   Returns an absolute path, or nil."
  [start]
  (loop [f (.getAbsoluteFile (File. (str start)))]
    (when f
      (if (some #(.exists (File. f ^String %)) markers)
        (.getPath f)
        (recur (.getParentFile f))))))

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
  (let [q (str/lower-case (str/trim input))]
    (if (empty? q)
      (vec (take 200 files))
      (->> files
           (filter #(subsequence? q (str/lower-case %)))
           (sort-by (fn [f]
                      (let [lf (str/lower-case f)
                            lb (str/lower-case (.getName (File. f)))]
                        [(if (= q lb) 0 1) (or (str/index-of lf q) 999) (count f)])))
           (take 200)
           vec))))