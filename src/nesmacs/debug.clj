(ns nesmacs.debug
  "Editor commands for the debug adapter."
  (:require [clojure.java.io :as io]
            [nesmacs.api :as api]
            [nesmacs.cmd :as cmd]
            [nesmacs.dap :as dap]
            [nesmacs.lang :as lang]
            [nesmacs.mini :as mini]
            [nesmacs.nav :as nav]
            [nesmacs.project :as project]
            [nesmacs.workspace :as workspace]))

(defn- debug-jump [s {:keys [file line]}]
  (if file (nav/jump s nil {:file file :line (some-> line inc)}) s))

(defn- debug-effect [s f]
  (api/after-command s (fn [publish!]
                         (when-let [result (f)]
                           (publish! cmd/msg result)))))

(defn start
  "C-c d d: start the language's debug adapter for the current buffer."
  [editor s]
  (let [file (:file (cmd/cur s))
        l (when file (lang/language-for-file file))
        cfg (:dap l)]
    (cond
      (nil? file) (cmd/msg s "Save the buffer first")
      (nil? l) (cmd/msg s "No language for this buffer")
      (nil? cfg) (cmd/msg s (str "No debug adapter for " (:id l) " (see nesmacs.lang :dap)"))
      (not (dap/available? l))
      (cmd/msg s (str "Debug adapter not installed: " (first (:argv cfg))))
      :else (debug-effect s #(dap/start! {:editor editor
                                          :root (or (project/root file)
                                                    (.getParent (java.io.File. ^String file)))
                                          :file file :language l :on-stop debug-jump})))))

(defn breakpoint
  "C-c d b: toggle a breakpoint on this line."
  [s]
  (if-let [file (:file (cmd/cur s))]
    (let [line (cmd/line-idx s)]
      (debug-effect s #(format "%s:%d — %d breakpoint(s)" file (inc line)
                               (count (dap/toggle-breakpoint! file line)))))
    (cmd/msg s "Buffer has no file")))

(defn project-debug [editor s]
  (if-let [root (project/context s)]
    (let [profiles (:launch (project/profile root))]
      (cond
        (and profiles (not (and (map? profiles) (every? simple-keyword? (keys profiles)))))
        (cmd/msg s "Project :launch must be a map keyed by keywords")
        (empty? profiles) (if (:file (cmd/cur s)) (start editor s)
                              (cmd/msg s "Open a source file or define :launch in .nesmacs.edn"))
        :else
        (mini/start s "Debug profile: "
          (fn [s input]
            (let [{:keys [program language request cwd args env arguments adapter-port] :or {request :launch}}
                  (get profiles (keyword input))
                  f (when (string? program) (io/file program))
                  file (when f (.getCanonicalPath (if (.isAbsolute f) f (io/file root program))))
                  lang (or (lang/language language) (lang/language-for-file file))]
              (cond
                (not (#{:launch :attach} request)) (cmd/msg s "Debug request must be :launch or :attach")
                (and adapter-port (not (and (= :attach request) (int? adapter-port) (<= 1 adapter-port 65535))))
                (cmd/msg s "Use :adapter-port from 1 to 65535 with :request :attach")
                (not (:dap lang)) (cmd/msg s "Choose a profile with a supported debugger")
                (and (= :launch request) (not (and file (.isFile (io/file file)))))
                (cmd/msg s "Debug program does not exist")
                (and args (not (and (vector? args) (every? string? args))))
                (cmd/msg s "Debug :args must be a vector of strings")
                (and env (not (and (map? env) (every? string? (keys env)) (every? string? (vals env)))))
                (cmd/msg s "Debug :env must map strings to strings")
                (and arguments (not (map? arguments))) (cmd/msg s "Debug :arguments must be a map")
                :else
                (let [arguments (cond-> arguments
                                  cwd (assoc :cwd (workspace/directory root cwd))
                                  args (assoc :args args)
                                  env (assoc :env env))]
                  (debug-effect s #(dap/start! {:editor editor :root root :file file :language lang
                                                :request (name request) :arguments arguments :adapter-port adapter-port
                                                :on-stop debug-jump}))))))
          (fn [input _] (mini/complete-prefix input (sort (map name (keys profiles))))))))
    (cmd/msg s "Open a project first")))

(defn continue [s] (debug-effect s dap/continue-run!))
(defn next-line     [s] (debug-effect s dap/next!))
(defn step-in  [s] (debug-effect s dap/step-in!))
(defn step-out [s] (debug-effect s dap/step-out!))

(defn quit
  "C-c d q: stop or detach from the debuggee."
  [s]
  (debug-effect s #(do (dap/stop!) "Debug session stopped")))

(defn stack
  "C-c d s: show the frames of the last stop."
  [s]
  (if (contains? (:bufs s) dap/stack-buffer-name)
    (cmd/set-cur-buffer s dap/stack-buffer-name)
    (cmd/msg s "No stack yet")))

(defn variables
  "C-c d v: re-fetch the locals of the stopped frame."
  [s]
  (debug-effect s dap/variables!))

(defn evaluate
  "C-c d e: evaluate an expression in the stopped frame."
  [s]
  (mini/start s "Debug eval: " (fn [s expr] (debug-effect s #(dap/evaluate! expr)))))
