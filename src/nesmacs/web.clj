(ns nesmacs.web
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [org.httpkit.server :as http]
            [nesmacs.log :as log]
            [nesmacs.term :as t]
            [nesmacs.view :as view])
  (:import [java.util.concurrent ArrayBlockingQueue]))

(def ^:private special-keys
  {"Tab" :tab "Enter" :return "Backspace" :backspace "Escape" :escape
   "ArrowUp" :up "ArrowDown" :down "ArrowLeft" :left "ArrowRight" :right
   "Home" :home "End" :end "Insert" :insert "Delete" :delete
   "PageUp" :page-up "PageDown" :page-down "F1" :f1 "F2" :f2 "F3" :f3 "F4" :f4})

(def ^:private max-text-length (* 1024 1024))

(defn- text-key [^String s]
  (if (= 1 (.codePointCount s 0 (.length s)))
    (if (= 1 (.length s)) (.charAt s 0) s)
    [:paste s]))

(defn- parse-event [msg]
  (when (and (string? msg) (<= (count msg) (+ 32 (* 6 max-text-length))))
    (let [event (json/read-str msg)]
      (when (vector? event)
        (let [[op a b c] event]
          (case op
            "key" (when (and (= 3 (count event)) (string? b))
                    (case a
                      "special" (special-keys b)
                      "ctrl" (when (= 1 (count b)) [:ctrl (first b)])
                      "meta" (when (= 1 (count b)) [:meta (first b)])
                      "meta-ctrl" (when (= 1 (count b)) [:meta [:ctrl (first b)]])
                      nil))
            "text" (when (and (= 2 (count event)) (string? a) (<= 1 (count a) max-text-length))
                     (text-key a))
            "paste" (when (and (= 2 (count event)) (string? a) (<= 1 (count a) max-text-length))
                      [:paste a])
            "resize" (when (and (= 3 (count event)) (int? a) (int? b)
                                (<= 2 a 300) (<= 2 b 500))
                       {:type :resize :rows a :cols b})
            "mouse" (when (and (= 4 (count event)) (nat-int? b) (nat-int? c)
                               (contains? #{"click" "up" "down"} a))
                      (if (= a "click")
                        {:type :mouse :action :press :button 0 :row b :col c}
                        {:type :mouse :action :scroll :direction (keyword a) :row b :col c}))
            nil))))))

(defn- render-frame [s]
  (let [out (StringBuilder.)
        result (binding [t/*out-fn* #(.append out (str %))]
                 (view/render s))]
    (assoc result :frame (str out))))

(defn render-to-string [s]
  (:frame (render-frame (dissoc s :render-sig))))

(defn- send! [ch packet]
  (try (http/send! ch (json/write-str packet))
       (catch java.io.IOException _ false)))

(defn- local-host? [host]
  (and host (re-matches #"(?i)(?:127\.0\.0\.1|localhost)(?::\d{1,5})?" host)))

(def ^:private assets
  {"/" ["client.html" "text/html"]
   "/client.js" ["client.js" "text/javascript"]
   "/vendor/xterm.css" ["vendor/xterm.css" "text/css"]
   "/vendor/xterm.js" ["vendor/xterm.js" "text/javascript"]
   "/vendor/addon-unicode11.js" ["vendor/addon-unicode11.js" "text/javascript"]
   "/vendor/addon-fit.js" ["vendor/addon-fit.js" "text/javascript"]})

(defn- serve-asset [path]
  (if-let [[file content-type] (assets path)]
    {:status 200
     :headers {"Content-Type" (str content-type "; charset=utf-8")
               "Content-Security-Policy" (str "default-src 'none'; script-src 'self'; "
                                              "style-src 'self' 'unsafe-inline'; connect-src 'self'; "
                                              "frame-ancestors 'none'")
               "X-Content-Type-Options" "nosniff"}
     :body (io/input-stream (io/resource (str "nesmacs/" file)))}
    {:status 404 :body "Not found"}))

(defn- ws-handler [clients ^ArrayBlockingQueue input req]
  (http/as-channel req
    {:on-open (fn [ch] (swap! clients assoc ch (atom {})))
     :on-close (fn [ch _] (swap! clients dissoc ch))
     :on-receive
     (fn [ch msg]
       (when-let [client (get @clients ch)]
         (try
           (if-let [event (parse-event msg)]
             ;; Reserve one queue slot per paste, regardless of its length.
             (when-not (.offer input [ch client event])
               (send! ch ["notice" "Input queue is full. Try again."]))
             (send! ch ["notice" "Unsupported editor input."]))
           (catch Exception e
             (log/log "web: invalid input:" (.getMessage e))))))}))

(defn- handler [clients input {:keys [uri headers request-method] :as req}]
  (let [host (get headers "host")]
    (cond
      (not (local-host? host)) {:status 403 :body "Local editor only"}
      (not= request-method :get) {:status 405 :headers {"Allow" "GET"}}
      (= uri "/ws") (if (= (str "http://" host) (get headers "origin"))
                      (ws-handler clients input req)
                      {:status 403 :body "Open the editor page to connect"})
      :else (serve-asset uri))))

(defn- apply-input! [commit-key! ch client event]
  (if (= :resize (:type event))
    (let [size (select-keys event [:rows :cols])]
      (when (not= size (:size @client))
        (reset! client {:size size})))
    (when-let [size (:size @client)]
      (let [mouse? (= :mouse (:type event))]
        (when (or (not mouse?) (:viewports @client))
          (commit-key! (if mouse? (assoc event :viewports (:viewports @client)) event)
                       size
                       #(when (<= (count %) 100000) (send! ch ["clipboard" %]))))))))

(defn- render-clients! [clients state]
  (doseq [[ch client] @clients]
    (let [{:keys [size sig]} @client]
      (when size
        (try
          (let [{:keys [frame sig viewports]}
                (render-frame (assoc (merge state size) :render-sig sig))]
            (when (seq frame)
              (if (send! ch ["frame" (:rows size) (:cols size) frame])
                (swap! client assoc :sig sig :viewports viewports)
                (swap! clients dissoc ch))))
          (catch Exception e
            (log/log "web: render error:" (.getMessage e))
            (swap! clients dissoc ch)
            (http/close ch)))))))

(defn start! [editor port commit-key!]
  (let [clients (atom {})
        input (ArrayBlockingQueue. 256)
        srv (http/run-server (partial handler clients input)
                             {:port port :ip "127.0.0.1"
                              :max-ws (+ 32 (* 6 max-text-length))})
        port (get (meta srv) :local-port port)]
    (log/log "web: listening on" port)
    {:port port
     :active? #(boolean (seq @clients))
     ;; HTTP workers only enqueue. The editor loop owns command execution and
     ;; renders after persisting edits and applying jobs, LSP results and themes.
     :input! (fn []
               (loop [remaining 64]
                 (when (and (pos? remaining) (not (:exit @editor)))
                   (when-let [[ch client event] (.poll input)]
                     (apply-input! commit-key! ch client event)
                     (recur (dec remaining))))))
     :render! #(render-clients! clients @editor)
     :stop! srv}))

(defn stop! [service]
  ((:stop! service)))
