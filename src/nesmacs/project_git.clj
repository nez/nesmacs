(ns nesmacs.project-git
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [nesmacs.buf :as buf]
            [nesmacs.cmd :as cmd]
            [nesmacs.files :as files]
            [nesmacs.git :as git]
            [nesmacs.job :as job]
            [nesmacs.lang :as lang]
            [nesmacs.listbuf :as listbuf]
            [nesmacs.mini :as mini]
            [nesmacs.proc :as proc]
            [nesmacs.project :as project]
            [nesmacs.window :as win]
            [nesmacs.workspace :as workspace]))

(defn- root [s]
  (git/repo-root (or (project/context s) (git/current-dir (cmd/cur s)))))

(defn- git-output [root args]
  (proc/run (into ["git"] args) root {:check-exit? true}))

(defn menu [s]
  (if-let [root (root s)]
    (let [name (workspace/buffer-name root "Git")]
      (listbuf/open s name {:name name :mode :project-mode :header root
                           :entries (cond-> [["Status and hunks" "git-status"] ["Switch branch" "project-git-switch"]
                                             ["Compare branch" "project-git-compare"] ["Resolve conflicts" "project-git-conflicts"]
                                             ["Pull (fast-forward)" "project-git-pull"] ["Push" "project-git-push"]]
                                      (get-in s [:commands "forge-prs"]) (conj ["Pull requests" "forge-prs"]))
                           :format-entry first :extras {:project-root (project/context s) :git-dir root}}))
    (cmd/msg s "Not a Git repository")))

(defn- repo-file? [root b]
  (when-let [file (:file b)] (.startsWith (.toPath (io/file file)) (.toPath (io/file root)))))

(defn- dirty? [s root]
  (some #(and (:modified %) (repo-file? root %)) (vals (:bufs s))))

(defn- refresh-after-git [root b]
  (when (zero? (:job-exit b -1))
    (fn [s _]
      (update s :bufs update-vals
        (fn [{:keys [file point version] :as old}]
          (if (and (repo-file? root old) (not (:modified old)) (.isFile (io/file file)))
            (try
              (let [fresh (files/read-document (:name old) file)]
                (if (= (:disk-revision old) (:disk-revision fresh)) old
                    (-> (lang/with-file-language fresh)
                        (assoc :mode (:mode old) :version (inc (or version 0)) :saved-revision (inc (or version 0))
                               :point (buf/clamp-pos fresh point)))))
              (catch Exception e (assoc old :reload-error (.getMessage e))))
            old))))))

(defn- run [s root label args refresh?]
  (if (and refresh? (dirty? s root))
    (cmd/msg s "Save or close modified repository buffers first")
    (job/start s {:argv (into ["git"] args) :dir root :buf-name (workspace/buffer-name root label)
                  :buffer-fields {:project-root root}
                  :on-finish (when refresh? (partial refresh-after-git root))})))

(defn- choose-branch [s prompt submit]
  (if-let [root (root s)]
    (let [refs (git-output root ["for-each-ref" "--format=%(refname:short)" "refs/heads"])
          branches (vec (remove str/blank? (str/split-lines refs)))]
      (cond
        (str/starts-with? refs "Error:") (cmd/msg s refs)
        (empty? branches) (cmd/msg s "No local branches")
        :else (mini/start s prompt
                (fn [s branch]
                  (if (some #{branch} branches) (submit s root branch) (cmd/msg s "Choose a local branch")))
                (fn [input _] (mini/complete-prefix input branches)))))
    (cmd/msg s "Not a Git repository")))

(defn switch-branch [s]
  (choose-branch s "Switch to branch: " #(run %1 %2 "Git switch" ["switch" "--" %3] true)))

(defn compare-branch [s]
  (choose-branch s "Compare HEAD with: " #(run %1 %2 "Git diff" ["diff" "--no-color" (str %3 "...HEAD") "--"] false)))

(defn pull [s]
  (if-let [root (root s)] (run s root "Git pull" ["pull" "--ff-only"] true)
    (cmd/msg s "Not a Git repository")))

(defn push [s]
  (if-let [root (root s)] (run s root "Git push" ["push"] false)
    (cmd/msg s "Not a Git repository")))

(defn- open-conflict [s root file]
  (let [path (.getCanonicalPath (io/file root file))
        s (files/find-file s path)]
    (if-not (= path (:file (cmd/cur s))) s
      (let [versions (mapv (fn [[n label]]
                             (let [key (workspace/buffer-name root (str label " " file))
                                   out (git-output root ["show" (str ":" n ":" file)])]
                               [key (assoc (buf/make key out nil) :title (str label " " file)
                                           :project-root root :read-only true :journal? false)]))
                           [[1 "Base"] [2 "Local"] [3 "Remote"]])
            [base local remote] (map (comp win/leaf first) versions)
            tree (win/split-node :stacked (win/leaf (:buf s))
                                 (win/split-node :side-by-side base (win/split-node :side-by-side local remote)))]
        (-> s (update :bufs into versions) (cmd/change-windows (fn [_ _] [tree [:a]])))))))

(defn conflicts [s]
  (if-let [root (root s)]
    (let [output (git-output root ["diff" "--name-only" "--diff-filter=U" "-z" "--"])
          files (vec (remove str/blank? (str/split output #"\u0000")))]
      (cond
        (str/starts-with? output "Error:") (cmd/msg s output)
        (empty? files) (cmd/msg s "No unresolved Git conflicts")
        :else (mini/start s "Conflicted file: "
                (fn [s file] (if (some #{file} files) (open-conflict s root file)
                                 (cmd/msg s "Choose a conflicted file")))
                (fn [input _] (mini/complete-prefix input files)))))
    (cmd/msg s "Not a Git repository")))
