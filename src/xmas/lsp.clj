(ns xmas.lsp
  "Minimal Language Server Protocol client: JSON-RPC 2.0 over stdio, using
   `xmas.proc` for the server process and `clojure.data.json` for the wire.

   Deliberately small: full-document sync, diagnostics as props, hover,
   definition, completion. Positions are UTF-16 code units (the LSP default),
   which is exactly what Java/Clojure strings index by, so no conversion.

   Servers are started on demand (first hover/definition/completion) and
   restarted lazily if the process dies. A slow server never blocks typing:
   requests are async and `tick!` reads output from the editor loop.

   shortcut: no rename/code-action/references/workspace-edit, no didClose on
   buffer kill (the server keeps docs until shutdown), and diagnostics are
   applied by absolute offset only (no incremental per-line tracking).
   Extend when a real workload needs it."
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            [xmas.gap :as gap]
            [xmas.proc :as proc]
            [xmas.props :as props]
            [xmas.log :as log])
  (:import [java.lang ProcessHandle]))

(defonce ^:private servers (atom {}))   ; root-id -> server map
(defonce ^:private next-id (atom 0))

;; --- Wire format ---

(defn- utf8-len ^long [^String s] (alength (.getBytes s "UTF-8")))

(defn encode-msg
  "Frame `msg` (a Clojure map) as an LSP message. Content-Length is bytes."
  [msg]
  (let [body (json/write-str msg)]
    (str "Content-Length: " (utf8-len body) "\r\n\r\n" body)))

(defn- slice-bytes [^bytes bs ^long from ^long to]
  (String. bs (int from) (int (- to from)) "UTF-8"))

(defn decode-msgs
  "Parse as many complete LSP messages as `buffer` holds. Returns
   [messages rest]. `buffer` is a String; Content-Length counts UTF-8 bytes."
  [^String buffer]
  (loop [buf buffer
         msgs []]
    (let [hp (str/index-of buf "\r\n\r\n")]
      (if hp
        (let [header (subs buf 0 hp)
              after  (subs buf (+ hp 4))
              lm     (re-find #"(?i)Content-Length:\s*(\d+)" header)
              have   (utf8-len after)]
          (if (and lm (>= have (Long/parseLong (nth lm 1))))
            (let [len   (Long/parseLong (nth lm 1))
                  bytes (.getBytes after "UTF-8")
                  body  (slice-bytes bytes 0 len)
                  rest* (slice-bytes bytes len (alength bytes))
                  msg   (try (json/read-str body {:key-fn keyword})
                             (catch Exception e
                               (log/log "LSP json parse:" (.getMessage e)) nil))]
              (recur rest* (cond-> msgs msg (conj msg))))
            [msgs buf]))
        [msgs buf]))))

;; --- Server handles ---

(defn- send! [h msg]
  (let [^java.io.OutputStream out (.getOutputStream ^Process (:process h))]
    (.write out (.getBytes (encode-msg msg) "UTF-8"))
    (.flush out)))

(defn- srv [root-id] (get @servers root-id))

(defn server-exists? [root-id] (contains? @servers root-id))

(defn ready?
  "True once the initialize handshake has completed, so feature requests are
   accepted. Sending them earlier is a protocol error."
  [root-id]
  (boolean (get-in @servers [root-id :initialized?])))

(defn- start! [root-id argv dir]
  (let [h (proc/start {:argv argv :dir dir :keep-stdin? true})]
    (swap! servers assoc root-id
           {:handle h :inbox (atom "") :pending {} :root dir
            :initialized? false :docs {}})))

(defn- drop! [root-id]
  (when-let [s (srv root-id)]
    (proc/kill! (:handle s))
    (swap! servers dissoc root-id)))

(defn- request! [root-id method params cb]
  (when-let [s (srv root-id)]
    (let [id (swap! next-id inc)]
      (swap! servers update root-id update :pending assoc id cb)
      (send! (:handle s) {:jsonrpc "2.0" :id id :method method :params params}))))

(defn- notify! [root-id method params]
  (when-let [s (srv root-id)]
    (send! (:handle s) {:jsonrpc "2.0" :method method :params params})))

;; --- Document sync ---

(defn- note-version! [root-id path version]
  (swap! servers update root-id update-in [:docs path] (constantly version)))

(defn- did-open! [root-id path text version]
  (notify! root-id "textDocument/didOpen"
           {:textDocument {:uri (str "file://" path)
                           :languageId "clojure"
                           :version version
                           :text text}})
  (note-version! root-id path version))

(defn- did-change! [root-id path text version]
  (notify! root-id "textDocument/didChange"
           {:textDocument {:uri (str "file://" path) :version version}
            :contentChanges [{:text text}]})
  (note-version! root-id path version))

(defn- sync-docs! [root-id state]
  (let [s (srv root-id)]
    (when (:initialized? s)
      (doseq [[_ b] (:bufs state)]
        (when-let [path (:file b)]
          (when (str/starts-with? path (:root s))
            (let [v (or (:version b) 0)
                  seen (get-in s [:docs path])]
              (cond
                (nil? seen)      (did-open! root-id path (str (:text b)) v)
                (not= seen v)    (did-change! root-id path (str (:text b)) v)))))))))

;; --- Diagnostics -> props ---

(defn- line-offset [text line]
  (when (< -1 line (gap/line-count text))
    (gap/nth-line-start text line)))

(defn- lsp-range->offsets [text {:keys [start end]}]
  (when-let [s (line-offset text (:line start))]
    (let [e (or (line-offset text (:line end)) (+ s (:character end)))]
      [(+ s (:character start 0))
       (max (+ s (:character start 0)) (+ e (:character end 0)))])))

(defn- diag-face [severity]
  (case severity 1 :diag-error 2 :diag-warning 3 :diag-info :diag-info))

(defn- apply-diagnostics! [editor path diags]
  (let [buf-key (first (keep (fn [[k b]] (when (= path (:file b)) k)) (:bufs @editor)))]
    (when buf-key
      (swap! editor update-in [:bufs buf-key]
             (fn [b]
               (let [text   (:text b)
                     ranges (for [{:keys [range severity message]} diags
                                  :let [offs (lsp-range->offsets text range)]
                                  :when offs]
                              (props/make (first offs) (second offs)
                                          (diag-face severity)
                                          {:owner :lsp :message message}))]
                 (assoc b :props (into (props/remove-owner (:props b) :lsp) ranges))))))))

(defn- on-notification [editor root-id {:keys [method params]}]
  (case method
    "textDocument/publishDiagnostics"
    (when params
      (apply-diagnostics! editor
                          (str/replace (:uri params) #"^file://" "")
                          (:diagnostics params)))
    "window/showMessage" (when-let [m (:message params)] (log/log "LSP:" root-id m))
    (log/log "LSP unhandled notification" root-id method)))

(defn- on-response [root-id {:keys [id result error]}]
  (when-let [s (srv root-id)]
    (if-let [cb (get-in s [:pending id])]
      (do (swap! servers update root-id update :pending dissoc id)
          (if error
            (log/log "LSP error" id error)
            (cb result)))
      (log/log "LSP response for unknown id" id))))

(defn- read-output! [editor root-id]
  (when-let [s (srv root-id)]
    (let [chunk (proc/drain! (:handle s) 100000)]
      (when (seq chunk)
        (swap! (:inbox s) str chunk))
      (let [[msgs rest*] (decode-msgs @(:inbox s))]
        (reset! (:inbox s) rest*)
        (doseq [msg msgs]
          (cond
            (:method msg) (on-notification editor root-id msg)
            (:id msg)     (on-response root-id msg)
            :else         (log/log "LSP unknown message" msg)))
        ;; Process died: drop it so the next request restarts the server.
        (when (proc/finished? (:handle s))
          (log/log "LSP server exited" root-id)
          (drop! root-id))))))

;; --- Public API ---

(defn- queue-init!
  "Queue `cb` to run once the initialize handshake completes."
  [root-id cb]
  (when cb
    (swap! servers update root-id update :after-init (fnil conj []) cb)))

(defn ensure!
  "Start the server for `root-id` if absent and run the initialize handshake.
   `cb` runs once the server is ready — immediately if it already is, otherwise
   queued behind the handshake. Safe to call repeatedly and concurrently."
  [root-id argv dir cb]
  (cond
    (ready? root-id)
    (when cb (cb nil))

    (server-exists? root-id)
    (queue-init! root-id cb)

    :else
    (do
      (start! root-id argv dir)
      (queue-init! root-id cb)
      (request! root-id "initialize"
                {:processId (.pid (ProcessHandle/current))
                 :rootUri (str "file://" dir)
                 :capabilities {:textDocument {:synchronization {:dynamicRegistration false}
                                               :hover {}
                                               :definition {}
                                               :completion {:completionItem {:snippetSupport false}}
                                               :publishDiagnostics {}}}}
                (fn [result]
                  (swap! servers update root-id assoc :initialized? true)
                  (notify! root-id "initialized" {})
                  (log/log "LSP initialized" root-id)
                  (let [cbs (:after-init (srv root-id))]
                    (swap! servers update root-id assoc :after-init [])
                    (doseq [c cbs] (c result))))))))

(defn- at-pos
  "Per-request document + position params, shared by hover/definition/completion."
  [path line char]
  {:textDocument {:uri (str "file://" path)}
   :position {:line line :character char}})

(defn hover [root-id path line char cb]
  (request! root-id "textDocument/hover" (at-pos path line char) cb))

(defn definition [root-id path line char cb]
  (request! root-id "textDocument/definition" (at-pos path line char) cb))

(defn completion [root-id path line char cb]
  (request! root-id "textDocument/completion" (at-pos path line char) cb))

(defn tick!
  "Editor-loop step: sync open documents and read server output. Applies
   diagnostics to buffers via `editor`. Never blocks."
  [editor]
  (let [state @editor]
    (doseq [[root-id _] @servers]
      (try (sync-docs! root-id state) (catch Exception e (log/log "LSP sync:" (.getMessage e))))
      (try (read-output! editor root-id) (catch Exception e (log/log "LSP read:" (.getMessage e)))))
    nil))

(defn shutdown-all! []
  (doseq [[root-id s] @servers]
    (try (send! (:handle s) {:jsonrpc "2.0" :method "exit" :params {}})
         (proc/kill! (:handle s))
         (catch Exception _))
    (swap! servers dissoc root-id)))