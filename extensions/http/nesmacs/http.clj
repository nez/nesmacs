(ns nesmacs.http
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
            [nesmacs.api :as api]
            [nesmacs.buf :as buf]
            [nesmacs.cmd :as cmd]
            [nesmacs.log :as log]))

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
    ;; A method is an HTTP token; anything else would reach http-kit as a bad
    ;; keyword. Only http(s) keeps the request from using another transport.
    (when (and method (re-matches #"[A-Za-z][A-Za-z-]*" method)
               url (re-matches #"(?i)https?://\S+" url))
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

(defn- response-cut
  "Index just after the request's blank line, or the end of the text when the
   user removed it."
  ^long [^String text]
  (if-let [i (str/index-of text "\n\n")]
    (+ i 2)
    (count text)))

(defn- with-response
  "State with `response` in the buffer's response area and `request-id` still
   the current one. Only that area is rewritten: a request edited while the
   reply was in flight stays as the user typed it. A reply for an older send
   is ignored so it cannot overwrite a newer one."
  [s name response request-id]
  (if-let [b (get-in s [:bufs name])]
    (if (not= request-id (:http-request b))
      s
      (let [old (str (:text b))
            cut (response-cut old)
            ;; Keep the blank line that separates request from response.
            sep (if (= cut (count old))
                  (cond (str/ends-with? old "\n\n") ""
                        (str/ends-with? old "\n") "\n"
                        :else "\n\n")
                  "")
            text (str (subs old 0 cut) sep response)]
        (assoc-in s [:bufs name]
                  (-> (buf/edit b 0 (count old) text)
                      (assoc :modified false)
                      (buf/set-point (constantly (min (long (:point b)) (count text))))))))
    s))

(defn http
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

(defn- send-request! [publish! {:keys [method url headers]} request-id]
  (let [finish! (fn [response]
                  (publish! with-response buffer-name
                            (render (assoc response :url url :method method)) request-id))]
    (try
      ((requiring-resolve 'org.httpkit.client/request)
       {:url url :method (keyword (str/lower-case method))
        :headers headers :timeout 15000}
       finish!)
      (catch Exception e
        (log/log "http:" (.getMessage e))
        (finish! {:error e})))))

(defn send-request [s]
  (if-let [request (parse-request (str (get-in s [:bufs buffer-name :text])))]
    (let [id (Object.)]
      (-> s
          (assoc-in [:bufs buffer-name :http-request] id)
          (with-response buffer-name "sending…\n" id)
          (cmd/msg (str (:method request) " " (:url request) " …"))
          (api/after-command #(send-request! % request id))))
    (cmd/msg s "No http(s) request line (expected: GET https://host/path)")))

(defn start! [_]
  (api/register-command! #'http)
  (api/register-mode! :http-mode :major {"C-c C-c" send-request}))
