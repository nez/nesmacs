(ns nesmacs.lsp-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [nesmacs.buf :as buf]
            [nesmacs.complete :as complete]
            [nesmacs.ed :as ed]
            [nesmacs.gap :as gap]
            [nesmacs.lsp :as lsp]
            [nesmacs.proc :as proc]
            [nesmacs.project :as project]
            [nesmacs.rpc :as rpc]
            [nesmacs.testutil :as tu]))

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
    (is (= path (lsp/uri->path (#'nesmacs.lsp/path->uri path)))))
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

(deftest nested-projects-sync-and-request-from-the-same-server
  (let [outer (tu/temp-dir "nesmacs-lsp-outer")
        inner (doto (java.io.File. outer "nested") .mkdir)
        outer-id (lsp/server-id (.getPath outer) "clojure")
        inner-id (lsp/server-id (.getPath inner) "clojure")
        outer-path (str outer "/outer.clj")
        inner-path (str inner "/inner.clj")
        state {:buf inner-path
               :bufs {outer-path (buf/make "outer.clj" "(outer)" outer-path)
                      inner-path (buf/make "inner.clj" "(inner)" inner-path)}}
        server (fn [id root] {:id id :root (.getPath ^java.io.File root)
                             :language-id "clojure" :initialized? true})
        servers (atom {outer-id (server outer-id outer) inner-id (server inner-id inner)})
        sent (atom [])
        scans (atom [])
        root-for project/root-for]
    (try
      (spit (java.io.File. outer "deps.edn") "{}")
      (spit (java.io.File. inner "deps.edn") "{}")
      (with-redefs-fn
        {#'nesmacs.lsp/servers servers
         #'ed/editor (atom state)
         #'project/root-for (fn [path] (swap! scans conj path) (root-for path))
         #'rpc/send! (fn [srv msg]
                       (swap! sent conj [(:id srv) (:method msg)
                                         (get-in msg [:params :textDocument :uri])]))
         #'rpc/read! (constantly [])
         #'proc/finished? (constantly false)
         #'lsp/on-path? (constantly true)}
        (fn []
          (lsp/tick! ed/editor)
          (lsp/tick! ed/editor)
          (ed/lsp-hover @ed/editor)
          (is (= #{[outer-id "textDocument/didOpen" (lsp/path->uri outer-path)]
                   [inner-id "textDocument/didOpen" (lsp/path->uri inner-path)]}
                 (set (take 2 @sent))))
          (is (= [[inner-id "textDocument/hover" (lsp/path->uri inner-path)]]
                 (drop 2 @sent)))
          (is (= {outer-path 1 inner-path 1} (frequencies @scans)))))
      (finally
        (lsp/close-doc! outer-path)
        (lsp/close-doc! inner-path)
        (tu/delete-tree outer)))))

(deftest document-completion-follows-buffer-lifetime
  (let [original {:buf "x" :bufs {"x" (buf/make "x" "foo" "/x")}}
        editor (atom original)
        replies (atom [])
        request #(complete/document-candidates! editor @editor %1 %2
                   (fn [reply] (swap! replies conj reply)))]
    (request [0 0] "")
    (request [0 0] "")
    (is (= 1 (count @replies)))
    (swap! editor update-in [:bufs "x"] buf/edit 0 0 "a")
    (request [0 0] "")
    ((second @replies) {:items [{:label "add"} {:label "other"}]})
    ((first @replies) [{:label "old"}])
    (is (= ["add"] (request [0 0] "ad")))
    (request [0 1] "")
    (reset! editor original)
    ((last @replies) [{:label "late"}])
    (is (= original @editor))))

(deftest completion-source-uses-snapshot-state
  ;; Sources receive {:state snapshot}, not the editor atom. Reading it as an
  ;; atom threw inside complete/start!'s try/catch, so LSP completion silently
  ;; returned [] forever.
  (let [path  (str (System/getProperty "user.dir") "/lsp-completion-test.clj")
        state {:buf "f" :bufs {"f" (assoc (buf/make "f" "(foo)" path) :point 1)}
               :lsp-command ["false"]}]
    (with-redefs [lsp/server-exists? (constantly true)
                  lsp/ready?         (constantly false)]
      (is (= [] (nesmacs.ed/lsp-completion-source "" {:state state}))))))

(deftest on-path-detects-installed-commands
  (is (lsp/on-path? ["sh"]))
  (is (false? (lsp/on-path? ["nesmacs-no-such-server-binary"])))
  ;; An explicit absolute path is checked directly, so a configured server
  ;; outside PATH still counts.
  (is (lsp/on-path? [(str (System/getProperty "java.home") "/bin/java")]))
  (is (not (lsp/on-path? []))))

(deftest change-range-is-the-smallest-single-edit
  (is (nil? (lsp/change-range "abc" "abc")))
  (testing "insertion at a point"
    (is (= {:range {:start {:line 0 :character 6} :end {:line 0 :character 6}}
            :text "brave "}
           (lsp/change-range "hello world" "hello brave world"))))
  (testing "deletion of a range"
    (is (= {:range {:start {:line 0 :character 6} :end {:line 0 :character 12}}
            :text ""}
           (lsp/change-range "hello brave world" "hello world"))))
  (testing "a new line: positions are UTF-16 offsets, not bytes"
    (is (= {:range {:start {:line 1 :character 0} :end {:line 1 :character 0}}
            :text "ñ\n"}
           (lsp/change-range "a\nb" "a\nñ\nb"))))
  (testing "changing the last character"
    (is (= {:range {:start {:line 0 :character 2} :end {:line 0 :character 3}}
            :text "z"}
           (lsp/change-range "abc" "abz")))))

(deftest workspace-edits-merges-a-document-listed-twice
  ;; Both groups carry offsets into the original text. Applying the first one
  ;; would shift the second, so they must be merged and sorted together.
  (let [paths (atom [])
        edit {:documentChanges
              [{:textDocument {:uri "file:///tmp/x.clj"}
                :edits [{:range {:start {:line 0 :character 4}
                                 :end {:line 0 :character 5}}
                         :newText "E"}]}
               {:textDocument {:uri "file:///tmp/../tmp/x.clj"}
                :edits [{:range {:start {:line 0 :character 1}
                                 :end {:line 0 :character 2}}
                         :newText "B"}]}]}
        groups (lsp/workspace-edits edit (fn [path]
                                         (swap! paths conj path)
                                         (gap/of "abcdef\n")))]
    (is (= [(.getCanonicalPath (java.io.File. "/tmp/x.clj"))] @paths))
    (is (= 1 (count groups)))
    (is (= [{:from 4 :to 5 :text "E"} {:from 1 :to 2 :text "B"}]
           (:edits (first groups))))))

(deftest range-offsets-clamp-lines-columns-and-eof
  (let [text (gap/of "a😀\nbc\nlast")]
    (is (= [3 6] (lsp/range->offsets text {:start {:line 0 :character 50}
                                          :end {:line 1 :character 50}})))
    (is (= [10 11] (lsp/range->offsets text {:start {:line 2 :character 3}
                                            :end {:line 3 :character 2}})))
    (is (nil? (lsp/range->offsets text {:start {:line 50 :character 2}
                                        :end {:line 1 :character -3}})))
    (is (= [7 7] (lsp/range->offsets text {:start {:line 2 :character -3}
                                          :end {:line 0 :character 0}})))
    (is (nil? (lsp/range->offsets text {:start {:line 0 :character "1"}
                                        :end {:line 0 :character 1}})))))
