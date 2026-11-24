(ns xmas.gitblame
  "Read-only blame listing for one file. RET jumps to the line in the
   source buffer. The `git blame` run is a job, so opening the listing never
   blocks the editor."
  (:require [xmas.cmd :as cmd]
            [xmas.deferred :as deferred]
            [xmas.git :as git]))

(defn- fmt [{:keys [hash author date text]}]
  (format "%s %s %-12s %s" hash date (or author "") text))

(defn- spec [buf-name path entries]
  {:name (str "*Blame " buf-name "*")
   :mode :nav-mode
   :header (str "Blame " path " (" (count entries) " lines)")
   :entries entries
   :format-entry fmt
   :extras {:nav-root nil}})

(defn open [s]
  (let [b (cmd/cur s)
        path (:file b)
        dir (git/current-dir b)]
    (cond
      (nil? path) (cmd/msg s "Buffer has no file")

      :else
      (if-let [root (git/repo-root dir)]
        (let [name (str "*Blame " (:name b) "*")]
          (deferred/start s
                          {:argv ["git" "blame" "--line-porcelain" "--"
                                  (git/path-arg root path)]
                           :dir root
                           :job-name "*git-blame*"
                           :build (fn [out]
                                    (let [entries (mapv #(assoc % :file path :path path)
                                                        (git/parse-blame out))]
                                      (if (empty? entries)
                                        "No blame (untracked or empty file)"
                                        [name (spec (:name b) path entries)])))}))
        (cmd/msg s "Not a git repository")))))