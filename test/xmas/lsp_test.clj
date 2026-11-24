(ns xmas.lsp-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [xmas.buf :as buf]
            [xmas.ed :as ed]
            [xmas.gap :as gap]
            [xmas.lsp :as lsp]))

(defn- body-of [framed]
  (let [i (.indexOf ^String framed "\r\n\r\n")]
    (subs framed (+ i 4))))

(deftest encode-msg-is-json-with-byte-length
  (testing "ASCII body round-trips and header counts bytes"
    (let [framed (lsp/encode-msg {:jsonrpc "2.0" :id 1 :method "initialize"})
          body   (body-of framed)]
      (is (re-find #"Content-Length: \d+\r\n\r\n" framed))
      (is (= {"jsonrpc" "2.0" "id" 1 "method" "initialize"}
             (json/read-str body))))))

(deftest content-length-counts-utf8-bytes
  (testing "a raw multibyte frame is sliced by byte count (as servers send it)"
    (let [body   "{\"id\":7,\"result\":{\"message\":\"café 日本\"}}"
          len    (alength (.getBytes body "UTF-8"))
          framed (str "Content-Length: " len "\r\n\r\n" body)
          [msgs rest*] (lsp/decode-msgs framed)]
      (is (> len (count body)))                 ; bytes exceed chars
      (is (= "café 日本" (get-in (first msgs) [:result :message])))
      (is (= "" rest*)))))

(deftest decode-round-trip
  (testing "encode -> decode yields the original map"
    (let [msg    {:jsonrpc "2.0" :id 3 :result {:contents "doc" :n [1 2 3]}}
          [msgs rest*] (lsp/decode-msgs (lsp/encode-msg msg))]
      (is (= [{:jsonrpc "2.0" :id 3 :result {:contents "doc" :n [1 2 3]}}] msgs))
      (is (= "" rest*)))))

(deftest decode-multiple-messages
  (testing "two frames in one buffer are both parsed"
    (let [a (lsp/encode-msg {:id 1 :method "a"})
          b (lsp/encode-msg {:id 2 :method "b"})
          [msgs rest*] (lsp/decode-msgs (str a b))]
      (is (= [1 2] (map :id msgs)))
      (is (= "" rest*)))))

(deftest decode-partial-waits
  (testing "an incomplete frame is left in the rest buffer"
    (let [framed (lsp/encode-msg {:id 9 :method "x"})
          half   (subs framed 0 (quot (count framed) 2))
          [msgs rest*] (lsp/decode-msgs half)]
      (is (empty? msgs))
      (is (= half rest*)))))

(deftest decode-multibyte-round-trip
  (testing "multibyte bodies survive a byte-correct slice"
    (let [framed (lsp/encode-msg {:id 4 :result {:text "héllo 世界"}})
          [msgs rest*] (lsp/decode-msgs framed)]
      (is (= "héllo 世界" (get-in (first msgs) [:result :text])))
      (is (= "" rest*)))))

(deftest uri-round-trip-covers-spaces-and-unicode
  (doseq [path ["/tmp/plain/x.clj"
                "/tmp/with space/x.clj"
                "/tmp/café/日本.clj"]]
    (is (= path (lsp/uri->path (#'xmas.lsp/path->uri path)))))
  (testing "a server URI with file:/// is decoded too"
    (is (= "/tmp/x.clj" (lsp/uri->path "file:///tmp/x.clj")))))

(deftest languages-with-servers-are-supported
  (is (lsp/supports? "src/foo.clj"))
  (is (lsp/supports? "foo.cljc"))
  (is (lsp/supports? "foo.cljs"))
  (is (lsp/supports? "deps.edn"))
  (is (lsp/supports? "app.py"))
  (is (lsp/supports? "app.tsx"))
  (is (not (lsp/supports? "init.el")))
  (is (not (lsp/supports? "README.md")))
  (is (not (lsp/supports? nil))))

(deftest lsp-commands-refuse-non-clojure-buffers
  (let [s {:buf "*test*"
           :bufs {"*test*" (assoc (buf/make "*test*" "x" nil)
                                  :file "/tmp/init.el")}
           :msg nil}]
    (is (= "No language server for this file" (:msg (ed/lsp-hover s))))))

(deftest diagnostic-ranges-clamp-at-end-and-drop-stale
  (let [offs (deref #'xmas.lsp/lsp-range->offsets)
        text (gap/of "abc\ndef\n")]
    (is (= [4 7] (offs text {:start {:line 1 :character 0}
                             :end   {:line 1 :character 3}})))
    (testing "end line == line-count means end of text"
      (is (= [4 8] (offs text {:start {:line 1 :character 0}
                               :end   {:line 3 :character 0}}))))
    (testing "a stale range beyond the text is dropped, not misapplied"
      (is (nil? (offs text {:start {:line 5 :character 0}
                            :end   {:line 6 :character 0}}))))))

;; The completion cache is what makes LSP completion non-blocking; guard it.
(deftest completion-cache-filters-and-resets-on-version
  (let [remember (deref #'xmas.ed/remember-completion!)
        cached   (deref #'xmas.ed/cached-completion)
        path     "/lsp-cache-test/x.clj"]
    (remember path 1 3 4 [{:label "add"} {:label "add-watch"} {:label "assoc"}])
    (testing "filters cached labels by prefix at the same version/position"
      (is (= ["add" "add-watch"] (cached path 1 3 4 "add")))
      (is (= []                   (cached path 1 3 4 "zzz"))))
    (testing "a version change invalidates old results"
      (is (nil? (cached path 2 3 4 "add")))
      (remember path 2 3 4 [{:label "new"}])
      (is (= ["new"] (cached path 2 3 4 "new"))))))

(deftest completion-cache-does-not-swallow-uncached-positions
  ;; `[]` is truthy, so an uncached position at a cached version used to fall
  ;; into the cache branch and never send a request.
  (let [remember (deref #'xmas.ed/remember-completion!)
        cached   (deref #'xmas.ed/cached-completion)
        path     "/lsp-cache-pos/x.clj"]
    (remember path 1 3 4 [{:label "add"}])
    (is (= ["add"] (cached path 1 3 4 "add")))
    (is (nil? (cached path 1 3 9 "add")))))

(deftest completion-source-uses-snapshot-state
  ;; Sources receive {:state snapshot}, not the editor atom. Reading it as an
  ;; atom threw inside complete/start!'s try/catch, so LSP completion silently
  ;; returned [] forever.
  (let [path  (str (System/getProperty "user.dir") "/lsp-completion-test.clj")
        state {:buf "f" :bufs {"f" (assoc (buf/make "f" "(foo)" path) :point 1)}
               :lsp-command ["false"]}]
    (with-redefs [lsp/server-exists? (constantly true)
                  lsp/ready?         (constantly false)]
      (is (= [] (xmas.ed/lsp-completion-source "" {:state state}))))))

(deftest on-path-detects-installed-commands
  (is (lsp/on-path? ["sh"]))
  (is (false? (lsp/on-path? ["xmas-no-such-server-binary"])))
  ;; An explicit absolute path is checked directly, so a configured server
  ;; outside PATH still counts.
  (is (lsp/on-path? [(str (System/getProperty "java.home") "/bin/java")]))
  (is (not (lsp/on-path? []))))
