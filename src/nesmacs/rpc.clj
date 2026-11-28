(ns nesmacs.rpc
  "The JSON-RPC base protocol shared by LSP and DAP: `Content-Length: N\\r\\n\\r\\n`
   followed by a JSON body, over a child process's stdio or a TCP socket.

   Only framing and transport live here. Each protocol owns its envelope
   (JSON-RPC `id`/`method` vs DAP `seq`/`type`) and its own request bookkeeping."
  (:require [clojure.data.json :as json]
            [nesmacs.log :as log]
            [nesmacs.proc :as proc])
  (:import [java.io InputStream OutputStream]
           [java.net InetSocketAddress Socket]
           [java.util Arrays]))

;; --- Wire format ---

(defn- utf8-len ^long [^String s] (alength (.getBytes s "UTF-8")))

(defn encode-msg
  "Frame `msg` (a Clojure map). Content-Length counts UTF-8 bytes."
  [msg]
  (let [body (json/write-str msg)]
    (str "Content-Length: " (utf8-len body) "\r\n\r\n" body)))

(defn- slice-bytes [^bytes bs ^long from ^long to]
  (String. bs (int from) (int (- to from)) "UTF-8"))

(defn- header-end [^bytes buffer from]
  (let [limit (- (alength buffer) 3)]
    (loop [i (int from)]
      (when (< i limit)
        (if (and (= 13 (aget buffer i)) (= 10 (aget buffer (+ i 1)))
                 (= 13 (aget buffer (+ i 2))) (= 10 (aget buffer (+ i 3))))
          i
          (recur (inc i)))))))

(defn- decode-bytes [^bytes buffer]
  (let [size (alength buffer)]
    (loop [from 0 messages []]
      (let [header (header-end buffer from)
            start (when header (+ header 4))
            length (when header
                     (some-> (re-find #"(?im)^Content-Length:[ \t]*(\d+)[ \t]*\r?$"
                                      (slice-bytes buffer from header))
                             second parse-long))]
        (if (and length (<= length (- size start)))
          (let [end (+ start length)
                msg (try (json/read-str (slice-bytes buffer start end) {:key-fn keyword})
                         (catch Exception e
                           (log/log "rpc json parse:" (.getMessage e))
                           nil))]
            (recur end (cond-> messages msg (conj msg))))
          [messages (if (zero? from) buffer (Arrays/copyOfRange buffer (int from) size))])))))

(defn decode-msgs
  "Parse complete frames from a string. Returns [messages rest]."
  [^String buffer]
  (let [[msgs remaining] (decode-bytes (.getBytes buffer "UTF-8"))]
    [msgs (String. ^bytes remaining "UTF-8")]))

;; --- Transports ---

(defn open-stdio
  "Start `argv` and talk to its stdio. The process is owned by `nesmacs.proc`, so
   `kill-all!` reaps it on exit."
  [{:keys [argv dir]}]
  {:kind :stdio
   :handle (proc/start {:argv argv :dir dir :keep-stdin? true :stderr :log :binary? true})
   :inbox (atom (byte-array 0))})

(defn start-adapter
  "Start a TCP adapter that prints its own output; returns the handle so the
   caller can kill it with the connection."
  [{:keys [argv dir]}]
  (proc/start {:argv argv :dir dir :stderr :merge :kind :dap}))

(defn- connect!
  [^String host ^long port ^long timeout-ms]
  (let [s (Socket.)]
    (try
      (.connect s (InetSocketAddress. host (int port)) (int timeout-ms))
      s
      (catch Exception e
        (try (.close s) (catch Exception _))
        e))))

(defn open-tcp
  "Connect to a DAP adapter listening on `port`. Retries for `timeout-ms`
   because the adapter needs a moment to bind."
  [{:keys [host port timeout-ms retry-ms]
    :or {host "127.0.0.1" timeout-ms 3000 retry-ms 50}}]
  (let [deadline (+ (System/currentTimeMillis) (long timeout-ms))]
    (loop []
      (let [s (connect! host port retry-ms)]
        (if (instance? Throwable s)
          (if (< (System/currentTimeMillis) deadline)
            (do (Thread/sleep (long retry-ms)) (recur))
            (throw ^Throwable s))
          {:kind :tcp :socket s
           :out (.getOutputStream ^Socket s)
           :in (.getInputStream ^Socket s)
           :inbox (atom (byte-array 0))})))))

(defn send!
  "Write one message, locking the stream so concurrent sends cannot interleave."
  [{:keys [out handle]} msg]
  (let [^OutputStream stream (or out (.getOutputStream ^Process (:process handle)))]
    (locking stream
      (.write stream (.getBytes (encode-msg msg) "UTF-8"))
      (.flush stream))))

(defn- read-tcp [^InputStream in]
  (let [n (min 100000 (.available in))]
    (when (pos? n)
      (let [buffer (byte-array n)
            got (.read in buffer)]
        (when (pos? got)
          (if (= got n) buffer (Arrays/copyOf buffer got)))))))

(defn read!
  "Read available frames without blocking the editor loop."
  [{:keys [kind in handle inbox]}]
  (let [^bytes chunk (if (= kind :tcp) (read-tcp in) (proc/drain-bytes! handle 100000))]
    (if (and chunk (pos? (alength chunk)))
      (let [^bytes pending @inbox
            size (alength pending)
            buffer (Arrays/copyOf pending (+ size (alength chunk)))]
        (System/arraycopy chunk 0 buffer size (alength chunk))
        (let [[msgs rest] (decode-bytes buffer)]
          (reset! inbox rest)
          msgs))
      [])))

(defn closed?
  [{:keys [kind handle socket]}]
  (case kind
    :stdio (proc/finished? handle)
    :tcp   (try (or (.isClosed ^Socket socket)
                    (.isInputShutdown ^Socket socket))
                (catch Exception _ true))
    false))

(defn close!
  [{:keys [kind handle socket]}]
  (case kind
    :stdio (proc/kill! handle)
    :tcp   (do (try (.close ^Socket socket) (catch Exception _))
               (when handle (proc/kill! handle)))))
