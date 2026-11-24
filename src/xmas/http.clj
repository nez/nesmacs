(ns xmas.http
  "A minimal HTTP client as an editor app: the request lives in a buffer.

   `M-x http` opens `*HTTP*` with a request template:

     GET https://example.com/
     Accept: text/plain

   `C-c C-c` parses everything above the first blank line (method, URL, headers)
   and sends it; the response replaces everything below. The app is the
   platform claim in one file: no HTTP UI, just a buffer, a mode, a keymap and a
   command.

   Bodies are capped so a huge response cannot make the editor crawl."
  (:require [clojure.string :as str]
            [org.httpkit.client :as http]
            [xmas.buf :as buf]
            [xmas.cmd :as cmd]
            [xmas.log :as log]))

(def ^:private buffer-name "*HTTP*")

(def ^:private template
  "GET https://example.com/\n\n")

(def ^:private max-body 200000)

;; --- Request ---

(defn parse-request
  "Parse the request part of a buffer (up to the first blank line):
   {:method \"GET\" :url \"...\" :headers {\"k\" \"v\"}}. nil when there is no
   request line or the URL is not http(s)."
  [^String text]
  (let [head (first (str/split text #"\r?\n\r?\n" 2))
        [first-line & header-lines] (str/split-lines head)
        [method url] (str/split (str/trim first-line) #"\s+" 2)]
    (when (and url (re-matches #"(?i)https?://\S+" url))
      {:method (str/upper-case method)
       :url url
       :headers (into {}
                      (keep (fn [line]
                              (let [[_ k v] (re-matches #"\s*([^:\s]+)\s*:\s*(.*?)\s*" line)]
                                (when k [k v]))))
                      header-lines)})))

;; --- Response ---

(defn render
  "Plain text for a response, or for a transport error. Header names are sorted
   so the same response always renders identically."
  [{:keys [status headers body error url method]}]
  (if error
    (str (or method "GET") " " url " failed: "
         (or (some-> ^Throwable error .getMessage) (str error)) "\n")
    (let [body (cond (nil? body) ""
                     (bytes? body) (str "<" (alength ^bytes body) " bytes>")
                     :else (let [s (str body)]
                             (if (> (count s) max-body)
                               (str (subs s 0 max-body)
                                    "\n\n[truncated: " (count s) " bytes]")
                               s)))]
      (str "HTTP " (or status "?") "\n"
           (apply str (for [[k v] (sort-by (comp str key) headers)]
                        (str (name k) ": "
                             (if (sequential? v) (str/join ", " v) v)
                             "\n")))
           "\n"
           (if (str/ends-with? body "\n") body (str body "\n"))))))

;; --- Buffers ---

(defn- with-request
  "State with the request on top and `response` below it. Point stays where the
   user had it while it was inside the request."
  [s name req response]
  (if-let [b (get-in s [:bufs name])]
    (let [old (str (:text b))
          point (long (:point b))
          tail (str/index-of old "\n\n")
          request (str (:method req) " " (:url req) "\n"
                       (apply str (for [[k v] (sort-by key (:headers req))]
                                    (str k ": " v "\n")))
                       "\n")
          new-point (if (and tail (<= point (inc (long tail)))) point (count request))]
      (assoc-in s [:bufs name]
                (-> (buf/edit b 0 (count old) (str request response))
                    (assoc :modified false)
                    (buf/set-point (constantly new-point)))))
    s))

(defn open
  "M-x http: show the request buffer, creating it with a template on first use.
   An edited request is never overwritten."
  [s]
  (if (contains? (:bufs s) buffer-name)
    (cmd/set-cur-buffer s buffer-name)
    (-> s
        (assoc-in [:bufs buffer-name]
                  (-> (buf/make buffer-name template nil)
                      (assoc :mode :http-mode :props [])))
        (cmd/set-cur-buffer buffer-name)
        (cmd/msg "C-c C-c sends the request above the blank line"))))

;; --- Sending ---

(defn send!
  "Send the request in `name`. Returns [state' message]: the state already says
   \"sending…\" and the response lands in the buffer later, from a future that
   swaps `editor-atom` (so the command itself stays pure)."
  ([state editor-atom] (send! state editor-atom buffer-name))
  ([state editor-atom name]
   (let [req (parse-request (str (get-in state [:bufs name :text])))]
     (if-not req
       [state "No http(s) request line (expected: GET https://host/path)"]
       (do
         (future
           (try
             (let [{:keys [status headers body error]}
                   @(http/request {:url (:url req)
                                   :method (keyword (str/lower-case (:method req)))
                                   :headers (:headers req)
                                   :timeout 15000})]
               (swap! editor-atom with-request name req
                      (render {:status status :headers headers :body body
                               :error error :url (:url req) :method (:method req)})))
             (catch Exception e
               (log/log "http:" (.getMessage e))
               (swap! editor-atom with-request name req
                      (render {:error e :url (:url req) :method (:method req)})))))
         [(with-request state name req "sending…\n")
          (str (:method req) " " (:url req) " …")])))))