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

(defn status
  "Porcelain v2 -z status. Returns a vector of maps:
   {:path :status :head :staged} where :status is :staged :unstaged :untracked
   and :staged boolean indicates if the index differs from HEAD."
  [dir]
  (let [root (repo-root dir)
        out (when root (run ["git" "status" "--porcelain=v2" "-z"] root))]
    (when (and root out)
      (let [lines (str/split out (re-pattern (str (char 0))))
            parsed (mapv (fn [line]
                           (let [parts (str/split line #"\s+" 3)]
                             (case (first parts)
                               "1" {:status :staged
                                    :staged true
                                    :path (second parts)}
                               "2" {:status :unstaged
                                    :staged false
                                    :path (second parts)}
                               "?" {:status :untracked
                                    :staged false
                                    :path (second parts)}
                               {:status :unknown})))
                         lines)
            filtered (filterv #(not= :unknown (:status %)) parsed)]
        filtered))))

(defn diff
  "Unified diff for `path` (relative to repo root). `staged?` true -> diff --cached."
  [dir path staged?]
  (let [root (repo-root dir)
        argv (-> ["git" "diff"]
                 (cond-> staged? (conj "--cached"))
                 (conj "--")
                 (conj path))]
    (if root
      (run argv root)
      "Not in a git repository")))

(defn add [dir path]
  (let [root (repo-root dir)]
    (if root
      (run ["git" "add" "--" path] root)
      "Not in a git repository")))

(defn reset [dir path]
  (let [root (repo-root dir)]
    (if root
      (run ["git" "reset" "HEAD" "--" path] root)
      "Not in a git repository")))

(defn commit
  "Commit with `message`. Returns output."
  [dir message]
  (let [root (repo-root dir)]
    (if root
      (run ["git" "commit" "-m" message] root)
      "Not in a git repository")))

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