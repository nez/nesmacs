(ns nesmacs.repl
  (:require [clojure.string :as str]
            [nesmacs.api :as api]
            [nesmacs.buf :as buf]
            [nesmacs.cmd :as cmd]
            [nesmacs.gap :as gap]
            [nesmacs.job :as job]
            [nesmacs.mini :as mini]
            [nesmacs.project :as project]
            [nesmacs.workspace :as workspace]
            [nrepl.core :as nrepl])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io StringReader Closeable]))

(defonce ^:private connections (atom {}))

(defn- close-connection! [root]
  (locking connections
    (when-let [connection (get @connections root)]
      (swap! connections dissoc root)
      (.close ^Closeable (:transport connection)))))

(defn close-all! []
  (doseq [root (keys @connections)] (close-connection! root)))

(defn- parse-port [value]
  (when-let [port (parse-long (str/trim (str value)))]
    (when (<= 1 port 65535) port)))

(defn connect [s]
  (if-let [root (project/context s)]
    (mini/start s "Project nREPL port on localhost: "
      (fn [s input]
        (if-let [port (parse-port input)]
          (-> s (assoc-in [:project-repl-ports root] port)
                (cmd/msg (str "Project nREPL set to localhost:" port)))
          (cmd/msg s "Enter a port from 1 to 65535"))))
    (cmd/msg s "Open a project first")))

(defn start [s]
  (if-let [root (project/context s)]
    (let [{:keys [argv cwd port] :or {argv ["clojure" "-M" "-m" "nrepl.cmdline" "--port" "0"]}}
          (:repl (project/profile root))
          name (workspace/buffer-name root "nREPL server")]
      (if (project/argv? argv)
        (-> s
            (update :project-repl-ports dissoc root)
            (cond-> (parse-port port) (assoc-in [:project-repl-ports root] (parse-port port)))
            (api/after-command (fn [_] (close-connection! root)))
            (job/start {:argv argv :dir (workspace/directory root cwd) :buf-name name
                        :buffer-fields {:project-root root}}))
        (cmd/msg s "Project :repl needs an :argv vector")))
    (cmd/msg s "Open a project first")))

(defn- repl-port [s root]
  (or (get-in s [:project-repl-ports root])
      (parse-port (get-in (project/profile root) [:repl :port]))
      (let [name (workspace/buffer-name root "nREPL server")
            server (cmd/buf s name)]
        (when (= :running (:job-status server))
          (some-> (re-find #"nREPL server started on port (\d+)" (str (:text server))) second parse-port)))))

(defn- connection! [root port]
  (locking connections
    (or (when (= port (get-in @connections [root :port])) (get @connections root))
        (do
          (close-connection! root)
          (let [transport (nrepl/connect :host "127.0.0.1" :port port)]
            (try
              (let [client (nrepl/client-session (nrepl/client transport 15000))
                    connection {:port port :transport transport :client client}]
                (swap! connections assoc root connection)
                connection)
              (catch Exception e (.close ^Closeable transport) (throw e))))))))

(defn- forms [text]
  (with-open [r (LineNumberingPushbackReader. (StringReader. (str text)))]
    (binding [*read-eval* false *data-readers* {} *default-data-reader-fn* tagged-literal
              *ns* (the-ns 'nesmacs.repl)]
      (let [offset #(min (count text) (+ (gap/nth-line-start text (dec (.getLineNumber r)))
                                         (dec (.getColumnNumber r))))]
        (loop [entries []]
          (let [start (offset)
                form (try (read {:eof ::eof :read-cond :preserve} r) (catch Exception _ ::eof))]
            (if (= form ::eof) entries
                (recur (conj entries [start (offset) form])))))))))

(defn- evaluation [s scope]
  (let [{:keys [text point file]} (cmd/cur s)
        forms (forms text)
        ns-form (some (fn [[_ _ f]] (when (and (seq? f) (= 'ns (first f)) (symbol? (second f))) (second f))) forms)
        [from to] (case scope
                    :buffer [0 (count text)]
                    :region (cmd/region-bounds s)
                    :form (or (some (fn [[from to]] (when (and (<= from point) (< point to)) [from to])) forms)
                              (when (= point (count text)) (peek forms))))]
    (when (and from to (< from to))
      (cond-> {:op "eval" :code (gap/substr text from to)}
        file (assoc :file file :line (inc (gap/line-of text from)))
        (and ns-form (not= :buffer scope)) (assoc :ns (str ns-form))))))

(defn- show-output [s root output]
  (let [name (workspace/buffer-name root "REPL")
        b (or (cmd/buf s name) (assoc (buf/make name) :project-root root :read-only true :journal? false))]
    (cond-> (assoc-in s [:bufs name] (buf/append-output b output))
      (= root (project/context s)) (cmd/msg (str "REPL: " (first (str/split-lines output)))))))

(defn output [s]
  (let [root (project/context s) name (workspace/buffer-name root "REPL")]
    (if (cmd/buf s name) (cmd/set-cur-buffer s name) (cmd/msg s "No project REPL output yet"))))

(defn evaluate [s scope]
  (if-let [root (project/context s)]
    (if-let [port (repl-port s root)]
      (if-let [request (evaluation s scope)]
        (api/after-command s
          (fn [publish!]
            (future
              (try
                (let [{:keys [client] :as c} (connection! root port)
                      result (locking c
                               (let [response (doall (nrepl/message client request))]
                                 (if (and (:ns request) (some #(some #{"namespace-not-found"} (:status %)) response))
                                   (do (dorun (nrepl/message client {:op "eval" :code (str "(require '" (:ns request) ")")}))
                                       (doall (nrepl/message client request)))
                                   response)))
                      out (apply str (map #(str (:out %) (:err %) (when-let [v (:value %)] (str v "\n"))) result))]
                  (publish! show-output root (if (seq out) out (str (mapv :status result) "\n"))))
                (catch Exception e (publish! cmd/msg-error "Project nREPL" e))))))
        (cmd/msg s "Select code to evaluate"))
      (cmd/msg s "Start or connect the project nREPL first"))
    (cmd/msg s "Open a project first")))
