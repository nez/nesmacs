(ns xmas.rpc
  "The JSON-RPC base protocol shared by LSP and DAP: `Content-Length: N\\r\\n\\r\\n`
   followed by a JSON body, over a child process's stdio or a TCP socket.

   Only framing and transport live here. Each protocol owns its envelope
   (JSON-RPC `id`/`method` vs DAP `seq`/`type`) and its own request bookkeeping."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [xmas.log :as log]
            [xmas.proc :as proc])
  (:import [java.io InputStream OutputStream]
           [java.net InetSocketAddress Socket]))

;; --- Wire format ---

(defn- utf8-len ^long [^String s] (alength (.getBytes s "UTF-8")))

(defn encode-msg
  "Frame `msg` (a Clojure map). Content-Length counts UTF-8 bytes."
  [msg]
  (let [body (json/write-str msg)]
    (str "Content-Length: " (utf8-len body) "\r\n\r\n" body)))

(defn- slice-bytes [^bytes bs ^long from ^long to]
  (String. bs (int from) (int (- to from)) "UTF-8"))

(defn decode-msgs
  "Parse as many complete messages as `buffer` holds. Returns [messages rest].
   `buffer` is a String; Content-Length counts UTF-8 bytes."
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
                               (log/log "rpc json parse:" (.getMessage e)) nil))]
              (recur rest* (cond-> msgs msg (conj msg))))
            [msgs buf]))
        [msgs buf]))))

;; --- Transports ---

(defn open-stdio
  "Start `argv` and talk to its stdio. The process is owned by `xmas.proc`, so
   `kill-all!` reaps it on exit."
  [{:keys [argv dir]}]
  (let [h (proc/start {:argv argv :dir dir :keep-stdin? true :stderr :log})]
    {:kind :stdio :handle h
     :out (.getOutputStream ^Process (:process h))
     :inbox (atom "")}))

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
           :inbox (atom "")})))))

(defn send!
  "Write one message. Small writes to a pipe; callers are protocol clients that
   send a few hundred bytes per request, never bulk data."
  [{:keys [^OutputStream out]} msg]
  (.write out (.getBytes (encode-msg msg) "UTF-8"))
  (.flush out))

(defn- read-tcp [^InputStream in]
  (let [n (.available in)]
    (if (pos? n)
      (let [buf (byte-array n)
            got (.read in buf)]
        (if (pos? got) (String. buf 0 got "UTF-8") ""))
      "")))

(defn read!
  "Non-blocking read of every complete message available. Never blocks the
   caller (the editor loop)."
  [{:keys [kind inbox] :as conn}]
  (let [chunk (if (= kind :tcp)
                (read-tcp (:in conn))
                (proc/drain! (:handle conn) 100000))]
    (when (seq chunk) (swap! inbox str chunk))
    (let [[msgs rest*] (decode-msgs @inbox)]
      (reset! inbox rest*)
      msgs)))

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