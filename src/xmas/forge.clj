(ns xmas.forge
  "Forge actions through the installed CLI (`gh` for GitHub, `glab` for GitLab).
   xmas reads their JSON output; it never talks to the API itself."
  (:require [clojure.data.json :as json]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [xmas.cmd :as cmd]
            [xmas.git :as git]
            [xmas.listbuf :as listbuf]))

(defn forge
  "Which CLI to use for `dir`: :github, :gitlab or nil."
  [dir]
  (when-let [root (git/repo-root dir)]
    (let [cfg (try (slurp (str root "/.git/config")) (catch Exception _ ""))]
      (cond
        (str/includes? cfg "github.com") :github
        (str/includes? cfg "gitlab") :gitlab
        :else nil))))

(defn parse-gh [s]
  (try
    (->> (json/read-str (str s) :key-fn keyword)
         (map (fn [pr]
                {:number (:number pr)
                 :title (:title pr)
                 :state (str/upper-case (str (:state pr)))
                 :branch (:headRefName pr)
                 :url (:url pr)}))
         vec)
    (catch Exception _ nil)))

(defn parse-glab [s]
  (try
    (->> (json/read-str (str s) :key-fn keyword)
         (map (fn [mr]
                {:number (:iid mr)
                 :title (:title mr)
                 :state (str/upper-case (str (:state mr)))
                 :branch (:source_branch mr)
                 :url (:web_url mr)}))
         vec)
    (catch Exception _ nil)))

(defn prs [dir]
  (case (forge dir)
    :github (let [{:keys [exit out]} (shell/sh "gh" "pr" "list" "--json"
                                               "number,title,state,headRefName,url"
                                               "--limit" "50" :dir dir)]
              (when (zero? (long exit)) (parse-gh out)))
    :gitlab (let [{:keys [exit out]} (shell/sh "glab" "mr" "list" "--output" "json"
                                               "--per-page" "50" :dir dir)]
              (when (zero? (long exit)) (parse-glab out)))
    nil))

(defn open-url [url]
  (try
    (.start (ProcessBuilder. (into-array String ["xdg-open" url])))
    true
    (catch Exception _ false)))

(defn- fmt [{:keys [number state branch title]}]
  (format "#%-5s %-8s %-24s %s" number state (or branch "") title))

(def ^:private prs-buf "*Pull Requests*")

(defn- spec [dir]
  {:name prs-buf
   :mode :forge-mode
   :header (str "Forge: " (clojure.core/name (or (forge dir) :unknown))
                "   RET=open in browser g=refresh")
   :entries (vec (prs dir))
   :format-entry fmt
   :extras {:git-dir dir}})

(defn open [s]
  (let [dir (git/current-dir (cmd/cur s))]
    (cond
      (nil? (forge dir)) (cmd/msg s "No GitHub/GitLab forge for this repository")
      :else (listbuf/open s prs-buf (spec dir)))))

(defn open-at-point [s]
  (if-let [e (listbuf/entry-at-point s)]
    (if (open-url (:url e))
      (cmd/msg s (str "Opened #" (:number e)))
      (cmd/msg s "Could not start xdg-open"))
    s))

(defn refresh [s]
  (let [b (cmd/cur s)]
    (listbuf/refresh s prs-buf (spec (:git-dir b)))))

(def forge-keymap
  {:return open-at-point
   \g refresh})