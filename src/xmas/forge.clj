(ns xmas.forge
  "Forge actions through the installed CLI (`gh` for GitHub, `glab` for GitLab).
   xmas reads their JSON output; it never talks to the API itself. Listing is a
   job, so a slow network call never blocks the editor."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [xmas.cmd :as cmd]
            [xmas.deferred :as deferred]
            [xmas.git :as git]
            [xmas.listbuf :as listbuf]))

(def ^:private prs-buf "*Pull Requests*")

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

(defn- prs-argv [kind]
  (case kind
    :github ["gh" "pr" "list" "--json" "number,title,state,headRefName,url"
             "--limit" "50"]
    :gitlab ["glab" "mr" "list" "--output" "json" "--per-page" "50"]))

(defn parse-prs [kind s]
  (case kind :github (parse-gh s) :gitlab (parse-glab s) nil))

(defn open-url [url]
  (try
    (.start (ProcessBuilder. (into-array String ["xdg-open" url])))
    true
    (catch Exception _ false)))

(defn- fmt [{:keys [number state branch title]}]
  (format "#%-5s %-8s %-24s %s" number state (or branch "") title))

(defn- spec [kind dir entries]
  {:name prs-buf
   :mode :forge-mode
   :header (str "Forge: " (name kind)
                "   RET=open in browser g=refresh")
   :entries entries
   :format-entry fmt
   :extras {:git-dir dir :forge-kind kind}})

(defn- open-deferred [s kind dir root]
  (deferred/start s
                  {:argv (prs-argv kind)
                   :dir root
                   :job-name "*forge*"
                   :build (fn [out]
                            (if-let [entries (parse-prs kind out)]
                              [prs-buf (spec kind dir entries)]
                              (str (name kind) " CLI failed or not installed")))}))

(defn open [s]
  (let [dir (git/current-dir (cmd/cur s))]
    (cond
      (nil? (forge dir)) (cmd/msg s "No GitHub/GitLab forge for this repository")
      :else (if-let [root (git/repo-root dir)]
              (open-deferred s (forge root) dir root)
              (cmd/msg s "Not a git repository")))))

(defn open-at-point [s]
  (if-let [e (listbuf/entry-at-point s)]
    (if (open-url (:url e))
      (cmd/msg s (str "Opened #" (:number e)))
      (cmd/msg s "Could not start xdg-open"))
    s))

(defn refresh [s]
  (let [b (cmd/cur s)
        kind (:forge-kind b)
        dir (:git-dir b)]
    (if (and kind dir)
      (open-deferred s kind dir (git/repo-root dir))
      s)))

(def forge-keymap
  {:return open-at-point
   \g refresh})