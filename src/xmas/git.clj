(ns xmas.git
  "Git porcelain wrappers. All functions return plain data; they never mutate
   the editor state. Callers are responsible for displaying results.
   Commands are argv vectors; no shell strings."
  (:require [clojure.string :as str])
  (:import [java.io File]))

(defn- run [argv dir]
  (let [pb (ProcessBuilder. (into-array String argv))
        _ (.directory pb (File. dir))]
    (try
      (let [p (.start pb)
            out (slurp (.getInputStream p) :encoding "UTF-8")
            _ (.waitFor p)]
        out)
      (catch Exception e (str "Error: " (.getMessage e))))))

(defn repo-root [dir]
  (let [out (run ["git" "rev-parse" "--show-toplevel"] dir)]
    (when-not (str/starts-with? out "Error:") (str/trim out))))

(defn current-dir
  "Best directory to run git in for a buffer: an explicit :git-dir, a job's
   :job-dir, the parent of its file, or the JVM cwd."
  [b]
  (or (:git-dir b) (:job-dir b)
      (some-> (:file b) java.io.File. .getParent)
      (System/getProperty "user.dir")))

(defn status
  "Porcelain v2 -z status. Returns a vector of maps:
   {:path :status :staged} where :status is :staged :unstaged :untracked
   and :staged boolean indicates if the index differs from HEAD."
  [dir]
  (let [root (repo-root dir)
        out (when root (run ["git" "status" "--porcelain=v2" "-z"] root))]
    (when (and root out)
      (->> (str/split out (re-pattern (str (char 0))))
           (keep (fn [line]
                   (let [parts (str/split line #"\s+" 3)
                         [status staged] (case (first parts)
                                           "1" [:staged true]
                                           "2" [:unstaged false]
                                           "?" [:untracked false]
                                           nil)]
                     (when status
                       {:status status :staged staged :path (second parts)}))))
           vec))))

(defn- in-root
  "Run `argv` in the repo containing `dir`, or return a plain error string."
  [dir argv]
  (if-let [root (repo-root dir)]
    (run argv root)
    "Not in a git repository"))

(defn add [dir path]
  (in-root dir ["git" "add" "--" path]))

(defn reset [dir path]
  (in-root dir ["git" "reset" "HEAD" "--" path]))

(defn commit
  "Commit with `message`. Returns output."
  [dir message]
  (in-root dir ["git" "commit" "-m" message]))

(defn log
  "Recent commits as a vector of {:hash :abbrev :subject :author :date}.
   `limit` max entries."
  [dir limit]
  (let [root (repo-root dir)]
    (when root
      (->> (run ["git" "log" (str "-n" limit) (str "--pretty=format:%H" (char 0) "%s" (char 0) "%an" (char 0) "%ad") "--date=short"] root)
           (str/split-lines)
           (map #(str/split % (re-pattern (str (char 0))) 4))
           (map (fn [[hash subject author date]]
                {:hash hash :abbrev (subs hash 0 7) :subject subject :author author :date date}))
           vec))))

(defn branch-name [dir]
  (let [root (repo-root dir)]
    (when root (str/trim (run ["git" "rev-parse" "--abbrev-ref" "HEAD"] root)))))

(defn is-repo? [dir] (boolean (repo-root dir)))