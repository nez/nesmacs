(ns nesmacs.lsp
  "Minimal Language Server Protocol client: JSON-RPC 2.0 over stdio, using
   `nesmacs.rpc` for the server transport and framing.

   Deliberately small: incremental text sync when the server advertises it
   (one minimal range per change, full document otherwise), diagnostics as
   props, hover, definition, completion, references, rename, code actions,
   signature help, document symbols and formatting. Positions are UTF-16 code
   units (the LSP default), which is exactly what Java/Clojure strings index
   by, so no conversion.

   Servers are started on demand (first request) and restarted lazily if the
   process dies. A slow server never blocks typing: requests are async and
   `tick!` reads output from the editor loop.

   One server per (project root, language): a Clojure and a Python server in
   the same project must not share a handle."
  (:require [clojure.string :as str]
            [nesmacs.buf :as buf]
            [nesmacs.gap :as gap]
            [nesmacs.lang :as lang]
            [nesmacs.proc :as proc]
            [nesmacs.project :as project]
            [nesmacs.props :as props]
            [nesmacs.rpc :as rpc]
            [nesmacs.log :as log])
  (:import [java.lang ProcessHandle]))

(defonce ^:private servers (atom {}))   ; root-id -> server map
(defonce ^:private next-id (atom 0))
(defonce ^:private document-roots (atom {}))

;; --- Wire format ---

(def encode-msg
  "See `nesmacs.rpc/encode-msg`. Kept here because the LSP client is the primary
   user of the framing helpers and its tests name them."
  rpc/encode-msg)

(def decode-msgs rpc/decode-msgs)

(defn path->uri
  "file: URI for `path`. Uses File.toURI so spaces and non-ASCII are escaped."
  [^String path]
  (str (.toURI (java.io.File. path))))

(defn uri->path
  "Inverse of `path->uri`; percent-decodes and handles file:/ and file:///."
  [uri]
  (try (.getPath (java.io.File. (java.net.URI. uri)))
       (catch Exception _ (str/replace uri #"^file://" ""))))

;; --- Server handles ---

(defn- srv [root-id] (get @servers root-id))

(defn server-exists? [root-id] (contains? @servers root-id))

(defn ready?
  "True once the initialize handshake has completed, so feature requests are
   accepted. Sending them earlier is a protocol error."
  [root-id]
  (boolean (get-in @servers [root-id :initialized?])))

(defn- start! [root-id argv dir language-id]
  (swap! servers assoc root-id
         (assoc (rpc/open-stdio {:argv argv :dir dir})
                :pending {} :root dir :language-id language-id
                :initialized? false :docs {} :texts {})))

(defn- drop! [root-id]
  (when-let [s (srv root-id)]
    (proc/kill! (:handle s))
    (swap! servers dissoc root-id)))

(defn- request! [root-id method params cb]
  (when-let [s (srv root-id)]
    (let [id (swap! next-id inc)]
      (swap! servers update root-id update :pending assoc id cb)
      (try (rpc/send! s {:jsonrpc "2.0" :id id :method method :params params})
           (catch Exception e
             (log/log "LSP request failed:" method (.getMessage e))
             (drop! root-id))))))

(defn- notify! [root-id method params]
  (when-let [s (srv root-id)]
    (try (rpc/send! s {:jsonrpc "2.0" :method method :params params})
         (catch Exception e
           (log/log "LSP notify failed:" method (.getMessage e))
           (drop! root-id)))))

;; --- Document sync ---

(defn- note-doc!
  "Record the version the server now holds, plus the text it was sent, so the
   next change can be sent as a minimal range."
  [root-id path version text]
  (swap! servers update root-id
         (fn [s] (-> s (assoc-in [:docs path] version)
                       (assoc-in [:texts path] text)))))

(defn- did-open! [root-id path text version language-id]
  (notify! root-id "textDocument/didOpen"
           {:textDocument {:uri (path->uri path)
                           :languageId language-id
                           :version version
                           :text text}})
  (note-doc! root-id path version text))

(defn- did-change! [root-id path text version]
  (notify! root-id "textDocument/didChange"
           {:textDocument {:uri (path->uri path) :version version}
            :contentChanges [{:text text}]})
  (note-doc! root-id path version text))

(defn change-range
  "Minimal single-range replacement turning `prev` into `next`, as one
   didChange contentChange. Nil when the texts are equal. The line/character
   positions are counted during the same scans that find the differing span, so
   a keystroke costs O(distance to it), not O(document)."
  [^String prev ^String next]
  (when (not= prev next)
    (let [p (count prev)
          n (count next)
          [from from-line from-col]
          (loop [i 0 line 0 col 0]
            (if (and (< i p) (< i n) (= (.charAt prev i) (.charAt next i)))
              (recur (inc i)
                     (if (= \newline (.charAt prev i)) (inc line) line)
                     (if (= \newline (.charAt prev i)) 0 (inc col)))
              [i line col]))
          suffix (loop [i 0]
                   (if (and (< i (- p from)) (< i (- n from))
                            (= (.charAt prev (- p i 1)) (.charAt next (- n i 1))))
                     (recur (inc i)) i))
          to (- p suffix)
          [to-line to-col]
          (loop [i from line from-line col from-col]
            (if (>= i to)
              [line col]
              (if (= \newline (.charAt prev i))
                (recur (inc i) (inc line) 0)
                (recur (inc i) line (inc col)))))]
      {:range {:start {:line from-line :character from-col}
               :end {:line to-line :character to-col}}
       :text (subs next from (- n suffix))})))

(defn- did-change-range!
  "Send one minimal change when the server asked for incremental sync. Falls
   back to a full document send when the previous text is unknown."
  [root-id path text version]
  (if-let [prev (get-in (srv root-id) [:texts path])]
    (do (when-let [chg (change-range prev text)]
          (notify! root-id "textDocument/didChange"
                   {:textDocument {:uri (path->uri path) :version version}
                    :contentChanges [chg]}))
        (note-doc! root-id path version text))
    (did-change! root-id path text version)))

(defn server-id
  "Opaque key for one server: project root plus language."
  [root language-id]
  (str root "|" language-id))

(defn supports?
  "True when `path` belongs to a language with a configured server."
  [path]
  (boolean (:lsp (lang/language-for-file path))))

(defn language-id-for [path]
  (:id (lang/language-for-file path)))

(defn document-session
  "Server identity for a document, using its nearest project or its own directory."
  [{:keys [file language large-file]}]
  (when (and file (not large-file))
    (let [language (or language (lang/language-for-file file))]
      (when (:lsp language)
        ;; shortcut: recheck project markers when the buffer reopens, add
        ;; filesystem invalidation if live project reconfiguration is needed.
        (let [root (or (get @document-roots file)
                       (let [root (or (project/root-for file)
                                      (.getParent (.getAbsoluteFile (java.io.File. ^String file))))]
                         (swap! document-roots assoc file root)
                         root))]
          {:root root :root-id (server-id root (:id language)) :language language})))))

(defn on-path?
  "True when the first element of a server `argv` is an executable on PATH."
  [argv]
  (proc/executable? argv))

(defn- sync-kind
  "TextDocumentSyncKind the server advertised: 0 none, 1 full, 2 incremental.
   Servers may send the number directly or a {:change n} options map."
  [capabilities]
  (let [s (:textDocumentSync capabilities)]
    (long (if (map? s) (long (or (:change s) 0)) (long (or s 0))))))

(defn- sync-document! [root-id s b]
  (let [path (:file b)
        version (or (:version b) 0)
        seen (get-in s [:docs path])]
    (cond
      (nil? seen) (did-open! root-id path (str (:text b)) version (:language-id s))
      (not= seen version)
      (if (= 2 (sync-kind (:capabilities s)))
        (did-change-range! root-id path (str (:text b)) version)
        (did-change! root-id path (str (:text b)) version)))))

(defn sync! [root-id buffer]
  (when-let [server (srv root-id)]
    (sync-document! root-id server buffer)))

(defn close-doc!
  "Notify every server that has `path` open that the document closed."
  [path]
  (swap! document-roots dissoc path)
  (doseq [[root-id s] @servers
          :when (get-in s [:docs path])]
    (notify! root-id "textDocument/didClose"
             {:textDocument {:uri (path->uri path)}})
    (swap! servers update root-id
           #(-> % (update :docs dissoc path) (update :texts dissoc path)))))

;; --- Diagnostics -> props ---

(defn position->offset
  "Convert an LSP UTF-16 position, clamping its column to the line."
  [text {:keys [line character] :or {character 0}}]
  (when (and (integer? line) (integer? character)
             (<= 0 line) (< line (gap/line-count text)))
    (let [start (gap/nth-line-start text line)
          end (gap/nth-line-end text line)]
      (+ start (min (- end start) (max 0 character))))))

(defn range->offsets
  "Convert an LSP range to bounded offsets. The line after the last may
   represent the end of the file in the end position."
  [text {:keys [start end]}]
  (let [from (position->offset text start)
        to (if (and (= (:line end) (gap/line-count text))
                    (integer? (:character end 0)))
             (count text)
             (position->offset text end))]
    (when (and from to)
      [from (max from to)])))

(defn- diag-face [severity]
  (case severity 1 :diag-error 2 :diag-warning 3 :diag-info :diag-info))

(defn- apply-diagnostics! [editor root path diags]
  (when path
    (swap! editor
           (fn [s]
             (let [s (assoc-in s [:project-diagnostics root path] diags)
                   buf-key (some (fn [[k b]] (when (= path (:file b)) k)) (:bufs s))]
               (if-not buf-key s
                 (update-in s [:bufs buf-key]
                   (fn [b]
               (let [text   (:text b)
                     ranges (for [{:keys [range severity message]} diags
                                  :let [offs (range->offsets text range)]
                                  :when offs]
                              (props/make (first offs) (second offs)
                                          (diag-face severity)
                                          {:owner :lsp :message message
                                           :severity severity}))]
                     (assoc b :props (props/add-all (props/remove-owner (:props b) :lsp) ranges)))))))))))

(defn- on-notification [editor root-id {:keys [method params]}]
  (case method
    "textDocument/publishDiagnostics"
    (when params
      (apply-diagnostics! editor (:root (srv root-id))
                          (uri->path (:uri params))
                          (:diagnostics params)))
    "window/showMessage" (when-let [m (:message params)] (log/log "LSP:" root-id m))
    (log/log "LSP unhandled notification" root-id method)))

(defn- on-response [editor root-id {:keys [id result error]}]
  (when-let [s (srv root-id)]
    (if-let [cb (get-in s [:pending id])]
      (do (swap! servers update root-id update :pending dissoc id)
          (if error
            (do (log/log "LSP error" id error)
                (when-not (:initialized? s)
                  (swap! editor assoc-in [:project-lsp-errors (:root s) (:language-id s)]
                         (or (:message error) "Initialization failed"))))
            ;; A throwing callback must not drop the messages decoded after it.
            (try (cb result)
                 (catch Exception e
                   (log/log "LSP callback:" (.getMessage e))))))
      (log/log "LSP response for unknown id" id))))

(defn- read-output! [editor root-id]
  (when-let [s (srv root-id)]
    (doseq [msg (rpc/read! s)]
      (cond
        (:method msg) (on-notification editor root-id msg)
        (:id msg)     (on-response editor root-id msg)
        :else         (log/log "LSP unknown message" msg)))
    ;; Process died: drop it so the next request restarts the server.
    (when (proc/finished? (:handle s))
      (log/log "LSP server exited" root-id)
      (drop! root-id))))

;; --- Public API ---

(defn- queue-init!
  "Queue `cb` to run once the initialize handshake completes."
  [root-id cb]
  (when cb
    (swap! servers update root-id update :after-init (fnil conj []) cb)))

(def ^:private start-lock (Object.))

(defn ensure!
  "Start the server for `root-id` if absent and run the initialize handshake.
   `cb` runs once the server is ready — immediately if it already is, otherwise
   queued behind the handshake. Safe to call repeatedly and concurrently: the
   check-then-start runs under a lock, so two callers cannot each spawn a
   server and leak one."
  ([root-id argv dir cb]
   (ensure! root-id argv dir "clojure" cb))
  ([root-id argv dir language-id cb]
   (let [ready-now?
         (locking start-lock
           (cond
             (ready? root-id) true
             (server-exists? root-id) (do (queue-init! root-id cb) false)
             :else
             (do
               (start! root-id argv dir language-id)
               (queue-init! root-id cb)
               (request! root-id "initialize"
                         {:processId (.pid (ProcessHandle/current))
                          :rootUri (path->uri dir)
                          :capabilities
                          {:textDocument
                           {:synchronization {:dynamicRegistration false}
                            :hover {}
                            :definition {}
                            :references {}
                            :rename {:prepareSupport false}
                            :codeAction {}
                            :signatureHelp {}
                            :documentSymbol {}
                            :formatting {}
                            :rangeFormatting {}
                            :completion {:completionItem {:snippetSupport true
                                                          :documentationFormat ["plaintext"]}}
                            :publishDiagnostics {}}
                           :workspace {:symbol {} :diagnostics {} :workspaceEdit {:documentChanges false}}}}
                         (fn [result]
                           (swap! servers update root-id
                                  assoc :initialized? true
                                  :capabilities (get-in result [:capabilities]))
                           (notify! root-id "initialized" {})
                           (log/log "LSP initialized" root-id)
                           (let [cbs (:after-init (srv root-id))]
                             (swap! servers update root-id assoc :after-init [])
                             (doseq [c cbs] (c result)))))
               false)))]
     (when (and ready-now? cb) (cb nil)))))

(defn- at-pos
  "Per-request document + position params, shared by hover/definition/completion."
  [path line char]
  {:textDocument {:uri (path->uri path)}
   :position {:line line :character char}})

(defn hover [root-id path line char cb]
  (request! root-id "textDocument/hover" (at-pos path line char) cb))

(defn definition [root-id path line char cb]
  (request! root-id "textDocument/definition" (at-pos path line char) cb))

(defn completion [root-id path line char cb]
  (request! root-id "textDocument/completion" (at-pos path line char) cb))

(defn references [root-id path line char cb]
  (request! root-id "textDocument/references"
            (assoc (at-pos path line char)
                   :context {:includeDeclaration true})
            cb))

(defn rename [root-id path line char new-name cb]
  (request! root-id "textDocument/rename"
            (assoc (at-pos path line char) :newName new-name)
            cb))

(defn code-action [root-id path range diagnostics cb]
  (request! root-id "textDocument/codeAction"
            {:textDocument {:uri (path->uri path)}
             :range range
             :context {:diagnostics (vec diagnostics)}}
            cb))

(defn signature-help [root-id path line char cb]
  (request! root-id "textDocument/signatureHelp" (at-pos path line char) cb))

(defn document-symbol [root-id path cb]
  (request! root-id "textDocument/documentSymbol"
            {:textDocument {:uri (path->uri path)}} cb))

(defn workspace-symbols [root-id query cb]
  (if (get-in (srv root-id) [:capabilities :workspaceSymbolProvider])
    (request! root-id "workspace/symbol" {:query query} cb)
    (cb nil)))

(defn workspace-diagnostics! [editor root on-result]
  (doseq [[id server] @servers
          :when (and (= root (:root server))
                     (get-in server [:capabilities :diagnosticProvider :workspaceDiagnostics]))]
    (request! id "workspace/diagnostic" {:previousResultIds []}
              (fn [{:keys [items]}]
                (doseq [{:keys [uri kind] :as item} items :when (= "full" kind)]
                  (apply-diagnostics! editor root (uri->path uri) (:items item)))
                (on-result)))))

(defn formatting [root-id path opts cb]
  (request! root-id "textDocument/formatting"
            {:textDocument {:uri (path->uri path)}
             :options (merge {:tabSize 2 :insertSpaces true} opts)}
            cb))

(defn range-formatting [root-id path range opts cb]
  (request! root-id "textDocument/rangeFormatting"
            {:textDocument {:uri (path->uri path)}
             :range range
             :options (merge {:tabSize 2 :insertSpaces true} opts)}
            cb))

(defn execute-command [root-id command args cb]
  (request! root-id "workspace/executeCommand"
            {:command command :arguments (vec args)} cb))

(defn workspace-edits
  "Group a WorkspaceEdit by canonical path and sort its absolute offsets
   in descending order. `text-for` reads the text once per canonical path.
   File create/rename/delete operations are ignored: nesmacs does not create or
   delete files behind the user's back."
  [edit text-for]
  (when (map? edit)
    (let [pairs (concat (for [[uri edits] (:changes edit)] [uri edits])
                        (for [d (:documentChanges edit)
                              :when (nil? (:kind d))]
                          [(get-in d [:textDocument :uri]) (:edits d)]))]
      (->> pairs
           (reduce (fn [by-path [uri edits]]
                     (if (and uri (seq edits))
                       (let [path (uri->path uri)
                             canonical (try (.getCanonicalPath (java.io.File. ^String path))
                                            (catch java.io.IOException _ path))]
                         (update by-path canonical (fnil into []) edits))
                       by-path))
                   {})
           (into []
                 (keep (fn [[path edits]]
                         (when-let [text (text-for path)]
                           (let [edits (keep (fn [{:keys [range newText]}]
                                               (when-let [[from to] (range->offsets text range)]
                                                 {:from from :to to
                                                  :text (buf/normalize-eol (str (or newText "")))}))
                                             edits)]
                             (when (seq edits)
                               {:path path :edits (vec (sort-by :from > edits))}))))))))))

(defn tick!
  "Editor-loop step: sync open documents and read server output. Applies
   diagnostics to buffers via `editor`. Never blocks."
  [editor]
  (when-let [running (seq @servers)]
    (let [documents (group-by (comp :root-id document-session) (vals (:bufs @editor)))]
      (doseq [[root-id s] running]
        (try
          (when (:initialized? s)
            (doseq [b (get documents (server-id (:root s) (:language-id s)))]
              (sync-document! root-id s b)))
          (catch Exception e (log/log "LSP sync:" (.getMessage e))))
        (try (read-output! editor root-id)
             (catch Exception e (log/log "LSP read:" (.getMessage e)))))))
  nil)

(defn shutdown-all! []
  (reset! document-roots {})
  (doseq [[root-id s] @servers]
    (try (rpc/send! s {:jsonrpc "2.0" :method "exit" :params {}})
         (proc/kill! (:handle s))
         (catch Exception _))
    (swap! servers dissoc root-id)))
