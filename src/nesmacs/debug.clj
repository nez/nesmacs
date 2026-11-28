(ns nesmacs.debug
  "Editor commands for the debug adapter."
  (:require [nesmacs.api :as api]
            [nesmacs.cmd :as cmd]
            [nesmacs.dap :as dap]
            [nesmacs.lang :as lang]
            [nesmacs.mini :as mini]
            [nesmacs.nav :as nav]
            [nesmacs.project :as project]))

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

(defn continue [s] (debug-effect s dap/continue-run!))
(defn next-line     [s] (debug-effect s dap/next!))
(defn step-in  [s] (debug-effect s dap/step-in!))
(defn step-out [s] (debug-effect s dap/step-out!))

(defn quit
  "C-c d q: disconnect and terminate the debuggee."
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
