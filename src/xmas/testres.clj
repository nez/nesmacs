(ns xmas.testres
  "Test-run output parsing: TAP, JUnit XML, the clojure.test/pytest summaries,
   or a generic file:line scan, into a list buffer of failures.

   `run-tests` starts the project's test command as a normal job. When the job
   finishes, `tick!` parses its output once and opens *Test Results*."
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

(defn parse-text
  "Failures and totals from the human output of `clojure.test` (cognitect
   test-runner) and `pytest`. Neither prints TAP or JUnit by default."
  [^String text]
  (let [clj-fails (mapv (fn [[_ kind name file line]]
                          {:ok false
                           :name name
                           :message (str file ":" line " " kind)})
                        (re-seq #"(?m)^(FAIL|ERROR) in \(([^)]*)\) \(([^:\s)]+):(\d+)\)" text))
        py-loc (re-seq #"(?m)^(\S+\.py):(\d+): (.+)$" text)
        py-fails (if (seq py-loc)
                   (mapv (fn [[_ file line msg]]
                           {:ok false :name msg :file file :line (Long/parseLong line)
                            :message (str file ":" line " " msg)})
                         py-loc)
                   (mapv (fn [[_ file test msg]]
                           {:ok false
                            :name test
                            :message (str file (when msg (str " " msg)))})
                         (re-seq #"(?m)^(?:FAILED|ERROR)\s+(\S+?)::(\S+)(?: - (.*))?$" text)))
        tests (into clj-fails py-fails)
        [total failed]
        (cond
          (re-find #"Ran \d+ tests" text)
          (let [t (some-> (re-find #"Ran (\d+) tests" text) second Long/parseLong)
                f (some->> [(re-find #"(\d+) failures?" text)
                            (re-find #"(\d+) errors?" text)]
                           (keep second) (map #(Long/parseLong %)) (reduce +))]
            [t f])

          (re-find #"\d+ (failed|passed)" text)
          (let [n (fn [w] (some-> (re-find (re-pattern (str "(\\d+) " w)) text)
                                  second Long/parseLong))
                f (+ (or (n "failed") 0) (or (n "error") 0) (or (n "errors") 0))]
            [(+ f (or (n "passed") 0)) f])

          :else [nil nil])]
    (when (and total (pos? (long total)))
      {:kind :text :tests tests :total total
       :failed (or failed (count tests))})))

(defn- count-of
  "First `N` immediately before one of the `words` (regex alternation)."
  [^String text words]
  (some->> (re-find (re-pattern (str "(?i)(\\d+)\\s+(?:" words ")\\b")) text)
           second Long/parseLong))

(defn- generic-failures
  "Lines that name a file:line and read like a failure. Deduplicated by
   location, so a runner that prints both a summary line and a traceback does
   not list the same failure twice."
  [^String text]
  (->> (str/split-lines text)
       (keep (fn [line]
               (when (re-find #"(?i)\b(fail(?:ed|ure|ing)?|error|exception|panic|assert)" line)
                 (when-let [[_ f l] (re-find #"([\w./-]+\.\w+):(\d+)" line)]
                   {:ok false :name (str/trim line)
                    :file f :line (Long/parseLong l)}))))
       (reduce (fn [acc e]
                 (if (some #(and (= (:file %) (:file e)) (= (:line %) (:line e))) acc)
                   acc (conj acc e)))
               [])))

(defn parse-generic
  "Last-resort parser for runners that print neither TAP, JUnit nor a known
   summary: any `file:line` line that mentions a failure, plus counts from a
   `N failed / N passed / N errors` summary. Keeps `run-tests` useful for an
   arbitrary command; the named entries are what the user can jump to."
  [^String text]
  (let [fails  (generic-failures text)
        failed (max (long (or (count-of text "failed|failures|errors?|failing") 0))
                    (count fails))
        passed (long (or (count-of text "passed|ok|succeeded") 0))]
    (when (pos? failed)
      {:kind :generic
       :tests fails
       :total (if (pos? passed) (+ passed failed) failed)
       :failed failed})))

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

    :else (let [known (parse-text text)
                generic (parse-generic text)]
            ;; Prefer the known format, but when it found counts and no
            ;; jumpable entries the generic scan still has file:line targets.
            (if (and known (seq (:tests known)))
              known
              (or generic known)))))

(defn- file-line
  "First file:line in a failure message, for jumping.
   Handles both relative and absolute paths."
  [^String message]
  (when message
    (when-let [[_ f l] (re-find #"([/\w./-]+\.\w+):(\d+)" message)]
      {:file f :line (Long/parseLong l)})))

(defn failure-entries [parsed]
  (vec (keep (fn [t]
               (when-not (:ok t)
                 (merge {:name (str/trim (str (:class t) " " (:name t)))
                         :message (:message t)}
                        (select-keys t [:file :line])
                        (file-line (:message t)))))
             (:tests parsed))))

(def ^:private results-name "*Test Results*")

(defn show-results
  [s parsed root]
  (let [entries (failure-entries parsed)]
    (-> (listbuf/open s results-name
                      {:name results-name
                       :mode :nav-mode
                       :header (str (:failed parsed) " failed / " (:total parsed)
                                    " tests (" (name (:kind parsed)) ")")
                       :entries entries
                       :format-entry (fn [{:keys [name message]}]
                                       (format "%s  %s" name (str/replace message #"\s+" " ")))
                       :extras {:nav-root root}})
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
        (if (and parsed (pos? (:total parsed)))
          (swap! editor show-results parsed (:test-root b))
          (swap! editor cmd/msg
                 (if (zero? (long (or (:job-exit b) -1)))
                   "Tests passed (exit 0); the output format had no failure lines"
                   "Test output not recognized (looked for TAP, JUnit, clojure.test, pytest, or file:line failures)")))))))

;; --- Command ---

(defn default-command [root]
  (cond
    (.exists (java.io.File. ^String root "deps.edn")) "clojure -M:test"
    (.exists (java.io.File. ^String root "pyproject.toml")) "pytest -q --tb=line"
    (.exists (java.io.File. ^String root "package.json")) "npm test"
    :else ""))

(defn run
  "Run `cmd` as a job whose output is parsed into *Test Results* on exit."
  [s cmd]
  (if (str/blank? cmd)
    (cmd/msg s "No test command")
    (let [b (cmd/cur s)
          root (or (project/root-for (:file b))
                   (System/getProperty "user.dir"))
          name "*Tests*"]
      (-> (job/start s {:argv ["sh" "-c" cmd] :dir root :buf-name name})
          (assoc-in [:bufs name :test-root] root)
          (assoc-in [:bufs name :test-report?] true)
          (assoc-in [:bufs name :test-parsed?] false)))))