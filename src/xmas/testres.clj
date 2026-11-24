(ns xmas.testres
  "Test-run output parsing: TAP or JUnit XML into a list buffer of failures.

   `run-tests` starts the project's test command as a normal job. When the job
   finishes, `tick!` parses its output once and opens *Test Results*; the
   command is the user's, so it must print TAP or JUnit XML for failures to be
   extracted (a summary line is still shown)."
  (:require [clojure.string :as str]
            [xmas.cmd :as cmd]
            [xmas.job :as job]
            [xmas.listbuf :as listbuf]
            [xmas.project :as project]))

;; --- Parsers ---

(defn parse-tap
  "TAP lines as {:ok :num :name}. Ignores the plan and diagnostics."
  [^String text]
  (->> (str/split-lines text)
       (keep (fn [line]
               (when-let [[_ not-ok num name] (re-find #"^\s*(not ok|ok)\s+(\d+)\s*-\s*(.*)$" line)]
                 {:ok (= not-ok "ok") :num num :name (str/trim name)})))
       vec))

(defn- attr [s k]
  (second (re-find (re-pattern (str k "=\"([^\"]*)\"")) s)))

(defn parse-junit
  "`<testcase>` elements as {:ok :name :class :message}. The message keeps the
   first file:line it mentions so failures can be jumped to."
  [^String text]
  (->> (re-seq #"(?s)<testcase\b([^>]*?)(?:/>|>(.*?)</testcase>)" text)
       (mapv (fn [[_ attrs body]]
               (let [body (or body "")
                     bad? (re-find #"<(failure|error)\b" body)
                     msg (or (some-> (re-find #"(?s)<(?:failure|error)\b[^>]*>(.*?)</(?:failure|error)>" body)
                                     second str/trim)
                             (attr (or (second (re-find #"(?s)<(?:failure|error)\b([^>]*)>" body)) "")
                                   "message")
                             "")]
                 {:ok (nil? bad?)
                  :name (attr attrs "name")
                  :class (attr attrs "classname")
                  :message msg})))
       vec))

(defn parse
  "Parse test output. Returns {:kind :tests :total :failed} or nil."
  [^String text]
  (cond
    (re-find #"(?s)<testcase\b" text)
    (let [tests (parse-junit text)]
      {:kind :junit :tests tests
       :total (count tests)
       :failed (count (remove :ok tests))})

    (re-find #"(?m)^\s*(not ok|ok)\s+\d+" text)
    (let [tests (parse-tap text)]
      {:kind :tap :tests tests
       :total (count tests)
       :failed (count (remove :ok tests))})

    :else nil))

(defn- file-line
  "First file:line in a failure message, for jumping."
  [^String message]
  (when message
    (when-let [[_ f l] (re-find #"([\w./-]+\.\w+):(\d+)" message)]
      {:file f :line (Long/parseLong l)})))

(defn failure-entries [parsed]
  (vec (keep (fn [t]
               (when-not (:ok t)
                 (merge {:name (str/trim (str (:class t) " " (:name t)))
                         :message (:message t)}
                        (file-line (:message t)))))
             (:tests parsed))))

(def ^:private results-name "*Test Results*")

(defn show-results
  [s parsed root]
  (let [entries (failure-entries parsed)]
    (-> s
        (assoc-in [:bufs results-name]
                  (listbuf/make
                    {:name results-name
                     :mode :nav-mode
                     :header (str (:failed parsed) " failed / " (:total parsed)
                                  " tests (" (name (:kind parsed)) ")")
                     :entries entries
                     :format-entry (fn [{:keys [name message]}]
                                     (format "%s  %s" name (str/replace message #"\s+" " ")))
                     :extras {:nav-root root}}))
        (cmd/set-cur-buffer results-name)
        (cmd/msg (str (:failed parsed) " failed / " (:total parsed) " tests")))))

(defn tick!
  "Editor-loop step: parse finished test jobs once."
  [editor]
  (let [s @editor
        pending (keep (fn [[name b]]
                        (when (and (:test-report? b)
                                   (= :done (:job-status b))
                                   (not (:test-parsed? b)))
                          [name b]))
                      (:bufs s))]
    (doseq [[name b] pending]
      (let [parsed (try (parse (str (:text b))) (catch Exception _ nil))]
        (swap! editor update-in [:bufs name] assoc :test-parsed? true)
        (when (and parsed (pos? (:total parsed)))
          (swap! editor show-results parsed (:test-root b)))))))

;; --- Command ---

(defn default-command [root]
  (cond
    (.exists (java.io.File. ^String root "deps.edn")) "clojure -M:test"
    (.exists (java.io.File. ^String root "pyproject.toml")) "pytest"
    (.exists (java.io.File. ^String root "package.json")) "npm test"
    :else ""))

(defn run
  "Run `cmd` as a job whose output is parsed into *Test Results* on exit."
  [s cmd]
  (if (str/blank? cmd)
    (cmd/msg s "No test command")
    (let [b (cmd/cur s)
          root (or (project/root (or (:file b) (System/getProperty "user.dir")))
                   (System/getProperty "user.dir"))
          name "*Tests*"]
      (-> (job/start s {:argv ["sh" "-c" cmd] :dir root :buf-name name})
          (assoc-in [:bufs name :test-root] root)
          (assoc-in [:bufs name :test-report?] true)
          (assoc-in [:bufs name :test-parsed?] false)))))