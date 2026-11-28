(ns nesmacs.workspace
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [nesmacs.cmd :as cmd]
            [nesmacs.dap :as dap]
            [nesmacs.job :as job]
            [nesmacs.lang :as lang]
            [nesmacs.listbuf :as listbuf]
            [nesmacs.lsp :as lsp]
            [nesmacs.api :as api]
            [nesmacs.mini :as mini]
            [nesmacs.project :as project]
            [nesmacs.proc :as proc]
            [nesmacs.terminal :as terminal]
            [nesmacs.testres :as testres]
            [nesmacs.window :as win]))

(defn buffer-name [root label]
  (str "*" label " " root "*"))

(defn- dashboard [root]
  {:name (buffer-name root "Project")
   :mode :project-mode
   :header root
   :entries [["Files" "project-find-file"] ["Search text" "project-search"]
             ["Symbols" "project-symbols"] ["Problems" "project-problems"]
             ["Tasks" "project-tasks"] ["Tests" "project-test"] ["Terminal" "project-terminal"]
             ["Debug" "project-debug"] ["REPL" "project-repl"] ["Git" "project-git"] ["Tools" "project-status"]]
   :format-entry first
   :extras {:project-root root}})

(defn menu [s]
  (if-let [root (project/context s)]
    (let [spec (dashboard root)] (listbuf/open s (:name spec) spec))
    (cmd/msg s "Open a project first")))

(defn restore-menus [s]
  (reduce-kv (fn [state key {:keys [project-root mode]}]
               (if (and (= :project-mode mode) project-root (= key (buffer-name project-root "Project")))
                 (listbuf/refresh state key (dashboard project-root))
                 state))
             s (:bufs s)))

(defn select-entry [s]
  (if-let [[_ command] (listbuf/entry-at-point s)]
    (if-let [run (get-in s [:commands command :fn])]
      (run s)
      (cmd/msg s (str "Command unavailable: " command)))
    s))

(defn activate [s directory]
  (let [dir (io/file directory)]
    (if-not (.isDirectory dir)
      (cmd/msg s (str "Project directory not found: " directory))
      (let [root (.getCanonicalPath dir)
            previous (:project-root s)
            spec (dashboard root)
            name (:name spec)
            s (cmd/save-cur-point s)
            s (cond-> s previous
                (assoc-in [:project-views previous] (select-keys s [:windows :cur-window])))
            {:keys [windows cur-window]} (get-in s [:project-views root])
            windows (reduce (fn [tree path]
                              (let [buffer (get-in tree (conj path :buffer))]
                                (if (or (= buffer name) (cmd/buf s buffer)) tree
                                    (update-in tree path win/set-buffer name))))
                            (or windows (win/leaf name)) (win/leaves (or windows (win/leaf name))))]
        (-> s
            (listbuf/refresh name spec)
            (assoc :project-root root)
            (update :project-recent #(vec (take 40 (cons root (remove #{root} %)))))
            (cmd/change-windows (fn [_ _] [windows (or cur-window [])])))))))

(defn open [s]
  (let [current (or (project/context s) (System/getProperty "user.dir"))
        recent (vec (distinct (cons current (:project-recent s))))]
    (mini/start s "Project directory: "
                (fn [s input] (activate s (if (str/blank? input) current input)))
                (fn [input s]
                  (let [matches (mini/complete-prefix input recent)]
                    (if (seq (:candidates matches)) matches (mini/file-completer input s)))))))

(defn directory [root cwd]
  (let [f (if cwd (io/file cwd) (io/file root))
        f (if (.isAbsolute f) f (io/file root cwd))]
    (when-not (.isDirectory f)
      (throw (ex-info (str "Directory not found: " f) {})))
    (.getCanonicalPath f)))

(defn task-specs [s root]
  (let [default (testres/default-command root)
        tasks (:tasks (project/profile root))]
    (when (and (some? tasks) (not (and (map? tasks) (every? simple-keyword? (keys tasks)))))
      (throw (ex-info "Project :tasks must be a map keyed by keywords" {})))
    (merge (when-not (str/blank? default)
             {:test {:argv (str/split default #" +") :report :tests}})
           (when (str/starts-with? default "pytest ")
             {:test-file {:argv ["pytest" "-q" "--tb=line" "{file}"] :report :tests}
              :test-failed {:argv ["pytest" "-q" "--tb=line" "--lf" "--last-failed-no-failures=none"] :report :tests}})
           tasks (get-in s [:project-task-overrides root]))))

(defn- task-argv [s root argv]
  (let [{:keys [file] :as b} (cmd/cur s)
        inputs {"{file}" (when file [file])
                "{line}" (when (:text b) [(str (inc (cmd/line-idx s)))])
                "{failed-files}" (into [] (comp (filter #(and (= root (:project-root %)) (= :tests (:task-report %))))
                                              (mapcat :problem-entries) (keep :file) (distinct)) (vals (:bufs s)))}]
    (into [] (mapcat (fn [arg]
                       (if (contains? inputs arg)
                         (or (seq (get inputs arg)) (throw (ex-info (str "Task requires " arg) {})))
                         [arg]))) argv)))

(defn run-task [s root task]
  (let [{:keys [argv cwd env interactive report]} (get (task-specs s root) task)]
    (cond
      (not (project/argv? argv)) (cmd/msg s (str "Task needs an :argv vector: " task))
      (and env (not (and (map? env) (every? string? (keys env)) (every? string? (vals env)))))
      (cmd/msg s "Task :env must map strings to strings")
      :else
      (let [dir (directory root cwd)
            argv (task-argv s root argv)]
        (if interactive
          (terminal/open s argv root dir env)
          (job/start s {:argv argv :dir dir :env env :buf-name (buffer-name root (str "Task " (name task)))
                        :mode :rg-mode :follow-output? false
                        :buffer-fields {:project-root root :task-report report}
                        :on-finish (partial testres/task-report root dir (buffer-name root (str "Results " (name task))) report)}))))))

(defn tasks [s]
  (if-let [root (project/context s)]
    (let [names (sort (map name (keys (task-specs s root))))]
      (if (seq names)
        (mini/start s "Project task: "
                    (fn [s input] (run-task s root (keyword input)))
                    (fn [input _] (mini/complete-prefix input names)))
        (cmd/msg s "Define tasks in .nesmacs.edn")))
    (cmd/msg s "Open a project first")))

(defn test-project [s]
  (if-let [root (project/context s)] (run-task s root :test) (cmd/msg s "Open a project first")))

(defn test-scope [s task]
  (if-let [root (project/context s)]
    (if (get (task-specs s root) task) (run-task s root task)
      (cmd/msg s (str "Define :" (name task) " in .nesmacs.edn")))
    (cmd/msg s "Open a project first")))

(defn terminal [s]
  (if-let [root (project/context s)]
    (terminal/open s (terminal/shell-argv) root root)
    (cmd/msg s "Open a project first")))

(defn- problem-list [s root focus?]
  (let [name (buffer-name root "Problems")
        lsp (for [[file items] (get-in s [:project-diagnostics root])
                  {:keys [range severity message]} items
                  :let [{:keys [line character]} (:start range)]
                  :when (int? line)]
              {:file file :line (inc line) :character character :message message :source "LSP" :severity severity})
        jobs (mapcat (fn [[key b]] (when (= root (:project-root b)) (map #(assoc % :source key) (:problem-entries b))))
                     (:bufs s))
        entries (vec (concat lsp jobs))
        spec {:name name :mode :project-problems-mode :header (str root " — " (count entries) " problems")
              :entries entries :extras {:project-root root :nav-root root}
              :format-entry (fn [{:keys [file line message source]}]
                              (str (when file (str file ":" line "  ")) (or message source)))}]
    ((if focus? listbuf/open listbuf/refresh) s name spec)))

(defn problems [editor s]
  (if-let [root (project/context s)]
    (api/after-command (problem-list s root true)
      (fn [publish!] (lsp/workspace-diagnostics! editor root #(publish! problem-list root false))))
    (cmd/msg s "Open a project first")))

(defn status [s]
  (if-let [root (project/context s)]
    (let [name (buffer-name root "Tools")
          languages (->> (concat (filter #(= root (project/root-for %)) (keep :file (vals (:bufs s))))
                                 (project/files root))
                         (keep lang/language-for-file) (distinct))
          tools (mapcat (fn [{:keys [id lsp formatter dap] :as language}]
                          (let [argv (or (get-in s [:lsp-commands id]) (:lsp-command s) lsp)
                                server (lsp/server-id root id)
                                health (cond
                                         (lsp/ready? server) " connected"
                                         (get-in s [:project-lsp-errors root id])
                                         (str " — " (get-in s [:project-lsp-errors root id]))
                                         (proc/executable? argv) " available"
                                         :else " missing")]
                            (cond-> []
                              lsp (conj (str id " LSP — " (first argv) health))
                              formatter (conj (str id " formatter — " (first (:argv formatter))
                                                   (if (proc/executable? (:argv formatter)) " available" " missing")))
                              dap (conj (str id " debugger — " (if (dap/available? language) "configured" "adapter missing"))))))
                        languages)
          tasks (for [[task {:keys [argv]}] (sort-by key (task-specs s root))]
                  (str (clojure.core/name task) " — " (if (project/argv? argv) (first argv) "invalid :argv")))]
      (listbuf/open s name {:name name :mode :fundamental :header root :entries (vec (concat tools tasks))
                           :format-entry identity :extras {:project-root root}}))
    (cmd/msg s "Open a project first")))
