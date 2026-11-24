(ns xmas.git
  "Git porcelain wrappers. All functions return plain data; they never mutate
   the editor state. Callers are responsible for displaying results.
   Commands are argv vectors; no shell strings."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]
           [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]))

(defn- run [argv dir]
  (let [pb (ProcessBuilder. (into-array String argv))
        _ (.directory pb (File. dir))]
    (try
      (let [p (.start pb)
            out (slurp (.getInputStream p) :encoding "UTF-8")
            _ (.waitFor p)]
        out)
      (catch Exception e (str "Error: " (.getMessage e))))))

(defn- run-stdin
  "Run `argv` feeding `input` on stdin. Used for `git apply`."
  [argv dir ^String input]
  (let [pb (ProcessBuilder. (into-array String argv))
        _ (.directory pb (File. dir))
        _ (.redirectErrorStream pb true)]
    (try
      (let [p (.start pb)]
        (with-open [w (io/writer (.getOutputStream p) :encoding "UTF-8")]
          (.write w input))
        (let [out (slurp (.getInputStream p) :encoding "UTF-8")]
          (.waitFor p)
          out))
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

;; --- Blame ---

(defn- fmt-date [^String epoch]
  (try (-> (Instant/ofEpochSecond (Long/parseLong epoch))
           (.atZone (ZoneId/systemDefault))
           (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd")))
       (catch Exception _ "")))

(defn parse-blame
  "`git blame --line-porcelain` output as {:line :hash :author :date :text}.
   The porcelain format repeats the headers for every line, so no grouping
   state is needed."
  [^String out]
  (loop [lines (str/split-lines out) cur {} acc []]
    (if-let [line (first lines)]
      (cond
        (re-matches #"[0-9a-f]{40} .*" line)
        (let [[sha _ final & _] (str/split line #"\s+")]
          (recur (rest lines)
                 {:hash (subs sha 0 8) :line (Long/parseLong final)}
                 acc))

        (str/starts-with? line "author ")
        (recur (rest lines) (assoc cur :author (subs line 7)) acc)

        (str/starts-with? line "author-time ")
        (recur (rest lines) (assoc cur :date (fmt-date (subs line 12))) acc)

        (str/starts-with? line "\t")
        (recur (rest lines) {}
               (conj acc (assoc cur :text (subs line 1))))

        :else (recur (rest lines) cur acc))
      acc)))

(defn path-arg
  "Path relative to the repo root, as git wants it for a pathspec."
  [^String root ^String path]
  (if (str/starts-with? path (str root java.io.File/separator))
    (subs path (inc (count root)))
    path))

;; --- Hunk staging ---

(defn parse-hunks
  "Split `git diff` output into the file header plus one map per hunk.
   Each map carries the full patch text needed to stage just that hunk."
  [^String out]
  (let [lines (str/split-lines out)
        header (vec (take-while #(not (str/starts-with? % "@@")) lines))
        hunks (reduce (fn [acc line]
                        (if (str/starts-with? line "@@")
                          (conj acc [line])
                          (if (seq acc)
                            (update acc (dec (count acc)) conj line)
                            acc)))
                      [] (drop-while #(not (str/starts-with? % "@@")) lines))]
    (map-indexed
      (fn [i hunk]
        {:index i
         :label (first hunk)
         :lines (vec hunk)
         :patch (str (str/join "\n" (concat header hunk)) "\n")})
      hunks)))

(defn diff-hunks
  "Unstaged hunks for `path` (what `git diff` shows)."
  [dir path]
  (let [root (repo-root dir)
        out (when root (run ["git" "diff" "--no-color" "--" path] root))]
    (when (and root out (not (str/starts-with? out "Error:")))
      (vec (parse-hunks out)))))

(defn staged-hunks
  "Staged hunks for `path` (what `git diff --cached` shows)."
  [dir path]
  (let [root (repo-root dir)
        out (when root (run ["git" "diff" "--cached" "--no-color" "--" path] root))]
    (when (and root out (not (str/starts-with? out "Error:")))
      (vec (parse-hunks out)))))

(defn apply-cached
  "Stage one hunk patch (or unstage it with `reverse?`)."
  [dir patch reverse?]
  (if-let [root (repo-root dir)]
    (run-stdin (cond-> ["git" "apply" "--cached"]
                 reverse? (conj "--reverse"))
               root patch)
    "Not in a git repository"))