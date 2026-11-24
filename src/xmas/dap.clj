(ns xmas.dap
  "Debug Adapter Protocol client on the JSON-RPC framing from `xmas.rpc`.

   One session at a time: a debuggee owns its process and its files, so a
   second concurrent session would have to pick which one stops. The adapter is
   data in `xmas.lang` (`:dap`): {:transport :stdio|:tcp, :argv [...],
   :launch {...}}. `{port}` in argv becomes a free port for a TCP adapter and
   `{env:NAME}` an environment variable, so the js-debug server path comes from
   $JS_DEBUG_DAP instead of a hard-coded location.

   Breakpoints live on the buffer's gutter, frames/locals/output in list
   buffers, evaluation in the minibuffer. Everything is async: `tick!` reads
   the adapter from the editor loop and commands return immediately, so a
   stopped debuggee never blocks typing."
  (:require [clojure.string :as str]
            [xmas.buf :as buf]
            [xmas.cmd :as cmd]
            [xmas.listbuf :as listbuf]
            [xmas.log :as log]
            [xmas.proc :as proc]
            [xmas.rpc :as rpc]))

(defonce ^:private session (atom nil))
(defonce ^:private breakpoints (atom {}))       ; path -> sorted set of 0-based lines
(defonce ^:private markers-version (atom 0))
(defonce ^:private markers-applied (atom -1))

(def ^:private stack-name "*Debug*")
(def ^:private locals-name "*Debug Locals*")
(def ^:private output-name "*Debug Output*")

;; --- Adapter configuration ---

(defn- expand-arg
  "Replace `{port}` and `{env:NAME}` in one adapter argument. nil means the
   configuration cannot be used (no port for stdio, unset variable)."
  [^String arg port]
  (cond
    (= arg "{port}") (when port (str port))
    (str/starts-with? arg "{env:")
    (let [k (subs arg 5 (dec (count arg)))]
      (System/getenv k))
    :else arg))

(defn adapter-argv
  "Concrete argv for a `:dap` config, or nil when an expansion is missing."
  [dap port]
  (let [argv (mapv #(expand-arg (str %) port) (:argv dap))]
    (when-not (some nil? argv) argv)))

(defn available?
  "True when the language's adapter can start: configured, argv expandable and
   its first element executable."
  ([language] (available? language nil))
  ([language port]
   (let [dap (:dap language)
         argv (when dap (adapter-argv dap (or port 0)))]
     (boolean (and argv (proc/executable? argv))))))

;; --- Protocol plumbing ---

(defn- message!
  "Set the echo-area message of the session's editor."
  [s text]
  (when-let [e (:editor s)] (swap! e cmd/msg text)))

(defn- request!
  "Send one DAP request. `cb` runs with the response body on success only."
  [s command args cb]
  (let [id (swap! (:seq s) inc)]
    (when cb (swap! (:pending s) assoc id cb))
    (try
      (rpc/send! (:conn s) {:seq id :type "request" :command command
                            :arguments (or args {})})
      (catch Exception e
        (log/log "DAP send failed:" command (.getMessage e))
        (when cb (swap! (:pending s) dissoc id))
        (str "Debug adapter error: " (.getMessage e))))))

(defn- reply-unsupported!
  "Adapter->client requests (runInTerminal, startDebugging) are out of scope;
   answering keeps the adapter from waiting on a timeout."
  [s msg]
  (try
    (rpc/send! (:conn s) {:seq (swap! (:seq s) inc)
                          :type "response"
                          :request_seq (:seq msg)
                          :command (:command msg)
                          :success false
                          :message "xmas does not support this request"})
    (catch Exception _)))

;; --- Breakpoints ---

(defn- dap-line [line] (inc (long (or line 0))))   ; 0-based buffer -> 1-based DAP

(defn- bump-markers! [] (swap! markers-version inc))

(defn- ordered
  "Keep the set small and sorted; the gutter and DAP both read it in order."
  [lines] (into (sorted-set) (filter nat-int?) lines))

(defn breakpoints-for [path] (get @breakpoints path #{}))

(defn send-breakpoints!
  "Send every file's breakpoints. Adapters tolerate a repeat, and repeating is
   what keeps the client authoritative after a restore or a new session."
  [s]
  (doseq [[path lines] @breakpoints]
    (request! s "setBreakpoints"
              {:source {:path path}
               :breakpoints (mapv (fn [l] {:line (dap-line l)}) lines)}
              nil)))

(defn set-breakpoints!
  "Replace the breakpoints of `path` (0-based lines). Used by session restore."
  [path lines]
  (let [ls (ordered lines)]
    (swap! breakpoints (fn [m] (if (seq ls) (assoc m path ls) (dissoc m path))))
    (bump-markers!)
    (when-let [s @session] (send-breakpoints! s))
    ls))

(defn restore-breakpoints!
  "Replace every breakpoint from persisted data {path [line ...]}."
  [m]
  (reset! breakpoints
          (into {}
                (for [[path lines] (or m {})
                      :when (and (string? path) (coll? lines))
                      :let [ls (ordered lines)]
                      :when (seq ls)]
                  [path ls])))
  (bump-markers!))

(defn snapshot-breakpoints [] @breakpoints)

(defn toggle-breakpoint!
  "Toggle `line` (0-based) in `path`. Returns the file's lines after the change."
  [path line]
  (if path
    (let [cur (breakpoints-for path)]
      (set-breakpoints! path (if (contains? cur line) (disj cur line) (conj cur line))))
    #{}))

;; --- Gutter ---

(defn gutter-for
  "The buffer's gutter: a dot per breakpoint, an arrow on the stopped line.
   nil when the buffer has neither, so most buffers pay nothing."
  [b stopped]
  (let [path (:file b)
        lines (get @breakpoints path)
        stop-line (when (and stopped path (= path (:file stopped))) (:line stopped))
        marks (cond-> (into {} (map (fn [l] [l {:text "●" :face :breakpoint}])) lines)
                (some? stop-line) (assoc stop-line {:text "▶" :face :debug-stop}))]
    (when (seq marks) {:width 1 :marks marks})))

(defn- apply-markers!
  "Rebuild every buffer's gutter from the breakpoint set and the stopped frame.
   Runs only when one of them changed."
  [editor-atom]
  (when (not= @markers-version @markers-applied)
    (reset! markers-applied @markers-version)
    (let [stopped (some-> @session :frame deref)]
      (swap! editor-atom update :bufs
             (fn [bufs]
               (into {}
                     (map (fn [[name b]]
                            [name (if-let [g (gutter-for b stopped)]
                                    (assoc b :gutter g)
                                    (dissoc b :gutter))]))
                     bufs))))))

;; --- Panels ---

(defn- put-buffer!
  "Install `b` under `name`, bumping its version so the renderer repaints even
   when the text is identical."
  [s name b]
  (swap! (:editor s) update :bufs
         (fn [bufs]
           (let [old (get bufs name)]
             (assoc bufs name
                    (assoc b :version (inc (long (or (:version old) 0)))))))))

(defn- read-only
  [b] (assoc b :read-only true :mode :fundamental :journal? false))

(defn- show-variables!
  "Render the locals collected so far into *Debug Locals*."
  [s]
  (let [lines (for [[scope pairs] @(:vars s)
                    :when (seq pairs)]
                (cons (str "[" scope "]")
                      (map (fn [[k v]] (format "  %-24s = %s" k v)) pairs)))
        text (str/join "\n" (apply concat lines))]
    (put-buffer! s locals-name (read-only (buf/make locals-name text nil)))))

(defn- fetch-variables!
  [s frame-id]
  (reset! (:vars s) {})
  (request! s "scopes" {:frameId frame-id}
            (fn [b]
              (let [scopes (:scopes b)]
                (if (empty? scopes)
                  (show-variables! s)
                  (doseq [sc scopes]
                    (request! s "variables"
                              {:variablesReference (:variablesReference sc)}
                              (fn [vb]
                                (swap! (:vars s) assoc (:name sc)
                                       (mapv (fn [v] [(str (:name v)) (str (:value v))])
                                             (:variables vb)))
                                (show-variables! s)))))))))

(defn- source-path
  "Absolute file of a stack frame, or nil for frames without one (adapter
   modules, generated code). A bare :name would not open anything."
  [frame]
  (let [p (get-in frame [:source :path])]
    (when (and p (.isAbsolute (java.io.File. ^String p))) p)))

(defn- build-frames!
  "Fill *Debug* with the stack. It does not steal focus from the source buffer;
   RET on an entry jumps there (nav-mode)."
  [s frames]
  (let [entries (->> frames
                     (keep (fn [f]
                             (when-let [path (source-path f)]
                               ;; DAP frame lines are already 1-based, which is
                               ;; what nav-jump/goto-line expect. The stopped
                               ;; frame keeps its 0-based line for the gutter.
                               {:file path
                                :line (long (or (:line f) 1))
                                :name (str (:name f))
                                :frame f})))
                     vec)]
    (when (seq entries)
      (put-buffer! s stack-name
                   (listbuf/make
                     {:name stack-name
                      :mode :nav-mode
                      :header (str (count entries) " frames — RET jumps")
                      :entries entries
                      :format-entry (fn [{:keys [name line file]}]
                                      (format "%-40s %s:%d" name file line))})))
    entries))

(defn- on-stopped!
  [s {:keys [reason threadId description text]}]
  (reset! (:state s) :stopped)
  (reset! (:thread-id s) threadId)
  (request! s "stackTrace" {:threadId threadId :startFrame 0 :levels 20}
            (fn [b]
              (let [frames (vec (:stackFrames b))
                    top (first frames)]
                (when top
                  (let [frame {:file (source-path top)
                               :line (dec (long (or (:line top) 1)))
                               :id (:id top)}]
                    (reset! (:frame s) frame)
                    (bump-markers!)
                    (when-let [cb (:on-stop s)] (swap! (:editor s) cb frame))))
                (build-frames! s frames)
                (when top (fetch-variables! s (:id top)))
                (message! s (str "Debugger stopped: " (or reason "pause")
                                 (when description (str " (" description ")"))
                                 (when text (str " " text))))))))

(defn- on-terminated! [s]
  (when-not (= :terminated @(:state s))
    (reset! (:state s) :terminated)
    (reset! (:frame s) nil)
    (reset! (:thread-id s) nil)
    ;; Adapters that listen on a port (js-debug) outlive the debuggee; close
    ;; and reap here instead of leaving a socket read failing every tick.
    (rpc/close! (:conn s))
    (when-let [a (:adapter s)] (proc/kill! a))
    (bump-markers!)
    (message! s "Debug session ended")))

(def ^:private output-limit 200000)

(defn- append-output!
  "A chatty debuggee would otherwise grow *Debug Output* without bound and
   re-copy the whole buffer on every event."
  [s body]
  (let [text (str (:output body))
        old  (str (:text (get-in @(:editor s) [:bufs output-name])))
        all  (str old text)
        kept (if (> (count all) output-limit)
               (str "[earlier output truncated]\n"
                    (subs all (- (count all) output-limit)))
               all)]
    (put-buffer! s output-name
                 (read-only (buf/make output-name kept nil)))))

(defn- on-event! [s msg]
  (case (:event msg)
    "initialized"
    (do (send-breakpoints! s)
        (request! s "setExceptionBreakpoints" {:filters []} nil)
        (request! s "configurationDone" nil nil)
        (reset! (:state s) :running)
        (message! s "Debugger attached"))

    "stopped" (on-stopped! s (:body msg))

    "continued" (do (reset! (:state s) :running)
                    (reset! (:frame s) nil)
                    (bump-markers!))

    ("terminated" "exited") (on-terminated! s)

    "output" (append-output! s (:body msg))

    "breakpoint" nil

    (log/log "DAP unhandled event" (:event msg))))

(defn- on-response! [s {:keys [request_seq success message body]}]
  (let [cb (get @(:pending s) request_seq)]
    (swap! (:pending s) dissoc request_seq)
    (if success
      ;; One bad callback must not abort the batch: later messages may include
      ;; the `stopped` event the whole UI is waiting for.
      (when cb
        (try (cb body)
             (catch Exception e (log/log "DAP response handler:" (.getMessage e)))))
      (do (log/log "DAP request failed:" request_seq message)
          (message! s (str "Debug adapter: " message))))))

(defn- on-message! [s msg]
  (case (:type msg)
    "response" (on-response! s msg)
    "event"    (on-event! s msg)
    "request"  (reply-unsupported! s msg)
    (log/log "DAP unknown message type" (:type msg))))

;; --- Session lifecycle ---

(defn running? [] (some? @session))

(defn state
  "Session state: :starting, :running, :stopped or :terminated."
  []
  (some-> @session :state deref))

(defn- initialize-args
  [{:keys [language]}]
  {:clientID "xmas"
   :clientName "xmas"
   :adapterID (:id language)
   :pathFormat "path"
   :linesStartAt1 true
   :columnsStartAt1 true
   :supportsVariableType true
   :supportsVariablePaging false
   :supportsRunInTerminalRequest false
   :supportsTerminateRequest true
   :supportsRestartRequest false})

(declare stop!)

(defn start!
  "Start a debug session for `file`. `opts`:
     :editor   editor atom
     :root     project directory
     :file     the file to debug
     :language language entry from `xmas.lang`
     :dap      adapter config, overriding the language's `:dap`
     :on-stop  (fn [state frame] state) — jump to the stopped frame
   Returns a message string for the caller to show."
  [{:keys [root file language dap] :as opts}]
  (let [dap (or dap (:dap language))]
    (cond
      (nil? dap)
      (str "No debug adapter for " (:id language) " (add :dap to xmas.lang)")

      :else
      (let [tcp? (= :tcp (:transport dap))
            port (when tcp? (proc/free-port))
            argv (adapter-argv dap port)]
        (cond
          (nil? argv)
          "Debug adapter needs an unavailable setting (check {env:...} in xmas.lang)"

          (not (proc/executable? argv))
          (str "Debug adapter not executable: " (first argv))

          :else
          (do
            (when @session (stop!))
            (let [started-adapter (atom nil)]
              (try
                (let [adapter (when tcp?
                                (let [a (rpc/start-adapter {:argv argv :dir root})]
                                  (reset! started-adapter a)
                                  a))
                      conn (if tcp?
                             (rpc/open-tcp {:port port})
                             (rpc/open-stdio {:argv argv :dir root}))
                    s (merge opts
                             {:conn conn :adapter adapter
                              :state (atom :starting) :seq (atom 0)
                              :pending (atom {}) :thread-id (atom nil)
                              :frame (atom nil) :vars (atom {})})]
                (reset! session s)
                (bump-markers!)
                (if-let [err (request! s "initialize" (initialize-args s)
                                       (fn [_]
                                         (request! s "launch"
                                                   (merge (:launch dap)
                                                          {:program file :cwd root})
                                                   nil)))]
                  (do (stop!) err)
                    (str "Starting " (:id language) " debugger...")))
                (catch Exception e
                  ;; `@session` is still nil when open-tcp throws, so the
                  ;; process started just above would otherwise be orphaned.
                  (when-let [a @started-adapter] (proc/kill! a))
                  (reset! session nil)
                  (str "Debug adapter failed to start: " (.getMessage e)))))))))))

(defn stop!
  "Disconnect (terminating the debuggee), close the transport and forget the
   session. Safe to call with no session. Async handlers (a terminate event)
   are the ones that report the end of the session to the user."
  []
  (when-let [s @session]
    (try (rpc/send! (:conn s) {:seq (swap! (:seq s) inc) :type "request"
                               :command "disconnect"
                               :arguments {:terminateDebuggee true}})
         (catch Exception _))
    (rpc/close! (:conn s))
    (when-let [a (:adapter s)] (proc/kill! a))
    (reset! session nil)
    (bump-markers!)))

(defn tick!
  "Editor-loop step: read the adapter, notice its exit, refresh gutters."
  [editor]
  (when-let [s @session]
    (when-not (= :terminated @(:state s))
      (try
        (doseq [msg (rpc/read! (:conn s))] (on-message! s msg))
        (catch Exception e (log/log "DAP read:" (.getMessage e))))
      (when (rpc/closed? (:conn s))
        (on-terminated! s))))
  ;; Apply marker changes that no other tick noticed (restore, stop).
  (apply-markers! editor))

(defn shutdown-all! [] (stop!))

;; --- Commands ---

(defn- resume!
  [command]
  (if-let [s @session]
    (if-let [tid @(:thread-id s)]
      (do (request! s command {:threadId tid} nil) nil)
      "Debugger is not stopped")
    "No debug session"))

(defn continue-run! [] (resume! "continue"))
(defn next!        [] (resume! "next"))
(defn step-in!     [] (resume! "stepIn"))
(defn step-out!    [] (resume! "stepOut"))

(def stack-buffer-name stack-name)

(defn variables!
  "Re-fetch locals for the stopped frame (e.g. after stepping)."
  []
  (if-let [s @session]
    (if-let [fid (some-> @(:frame s) :id)]
      (do (fetch-variables! s fid) nil)
      "Debugger is not stopped")
    "No debug session"))

(defn evaluate!
  "Evaluate `expr` in the stopped frame and echo the result."
  [expr]
  (if-let [s @session]
    (do (request! s "evaluate"
                  (cond-> {:expression expr :context "repl"}
                    (some-> @(:frame s) :id) (assoc :frameId (:id @(:frame s))))
                  (fn [b] (message! s (str "=> " (:result b)))))
        nil)
    "No debug session"))

(defn status-line []
  (if-let [s @session]
    (str "debug: " (name @(:state s))
         (when-let [f @(:frame s)]
           (str " at " (:file f) ":" (inc (long (:line f))))))
    "debug: off"))