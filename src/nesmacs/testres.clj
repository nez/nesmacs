(ns nesmacs.testres
  "Test-run output parsing: TAP, JUnit XML, the clojure.test/pytest summaries,
   or a generic file:line scan, into a list buffer of failures.

   `run-tests` starts the project's test command as a normal job. When the job
   finishes, `tick!` parses its output once and opens *Test Results*."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [nesmacs.cmd :as cmd]
            [nesmacs.job :as job]
            [nesmacs.listbuf :as listbuf]
            [nesmacs.project :as project]))

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
                  :message msg})))))

(defn parse-text
  "Failures and totals from the human output of `clojure.test` (cognitect
   test-runner) and `pytest`. Neither prints TAP or JUnit by default."
  [^String text]
  (let [clj-fails (mapv (fn [[_ kind name file line]]
                          {:ok false
                           :name name
                           :message (str file ":" line " " kind)})
                        (re-seq #"(?m)^(FAIL|ERROR) in \(([^)]*)\) \(([^\r\n]+):(\d+)\)" text))
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

(defn- file-line
  "First file:line in a failure message, for jumping."
  [^String message]
  (when message
    (when-let [[_ file line] (re-find #"([\w./-]+\.\w+):(\d+)" message)]
      {:file file :line (Long/parseLong line)})))

(defn- generic-failures
  "Lines that name a file:line and read like a failure. Deduplicated by
   location, so a runner that prints both a summary line and a traceback does
   not list the same failure twice."
  [^String text]
  (->> (str/split-lines text)
       (keep (fn [line]
               (when (re-find #"(?i)\b(fail(?:ed|ure|ing)?|error|exception|panic|assert)" line)
                 (when-let [location (file-line line)]
                   (assoc location :ok false :name (str/trim line))))))
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

(defn- summarize [kind tests]
  {:kind kind :tests tests :total (count tests) :failed (count (remove :ok tests))})

(defn parse
  "Parse test output. Returns {:kind :tests :total :failed} or nil."
  [^String text]
  (cond
    (re-find #"(?s)<testcase\b" text)
    (summarize :junit (parse-junit text))

    (re-find #"(?m)^\s*(not ok|ok)\s+\d+" text)
    (summarize :tap (parse-tap text))

    :else (let [known (parse-text text)
                generic (parse-generic text)]
            ;; Prefer the known format, but when it found counts and no
            ;; jumpable entries the generic scan still has file:line targets.
            (if (and known (seq (:tests known)))
              known
              (or generic known)))))

(defn failure-entries [parsed]
  (vec (keep (fn [t]
               (when-not (:ok t)
                 (merge {:name (str/trim (str (:class t) " " (:name t)))
                         :message (:message t)}
                        (file-line (:message t))
                        (select-keys t [:file :line]))))
             (:tests parsed))))

(defn- resolve-locations [root entries]
  ;; clojure.test can report a classpath-relative path or just a filename.
  (let [by-name (delay (group-by #(.getName (io/file %)) (project/files root)))]
    (mapv (fn [{:keys [file] :as entry}]
            (if-let [path (some-> file io/file)]
              (let [absolute? (.isAbsolute path)
                    target (if absolute? path (io/file root file))
                    basename (.getName path)
                    exists? (.isFile target)
                    matches (when-not (or absolute? exists?)
                              (filter #(or (= file basename)
                                           (str/ends-with? % (str "/" file)))
                                      (get @by-name basename)))]
                (cond
                  exists? entry
                  (= 1 (count matches)) (assoc entry :file (first matches))
                  :else (assoc (dissoc entry :file) :location-error
                               (if (seq matches)
                                 (str "Multiple project files match " file)
                                 (str "Test source not found: " file)))))
              entry))
          entries)))

(def ^:private results-name "*Test Results*")

(defn show-results
  ([s parsed root]
   (show-results s parsed root (resolve-locations root (failure-entries parsed))))
  ([s parsed root entries]
   (-> (listbuf/open s results-name
                     {:name results-name
                      :mode :nav-mode
                      :header (str (:failed parsed) " failed / " (:total parsed)
                                   " tests (" (name (:kind parsed)) ")"
                                   (when (seq entries) "   C-n select  RET jump"))
                      :entries entries
                      :format-entry (fn [{:keys [name message]}]
                                      (format "%s  %s" name (str/replace (or message "") #"\s+" " ")))
                      :extras {:nav-root root}})
       (cmd/msg (str (:failed parsed) " failed / " (:total parsed) " tests")))))

(defn- finish-report [root b]
  (if (= :failed (:job-status b))
    (fn [s _] (cmd/msg s (first (str/split-lines (str (:text b))))))
    (let [parsed (parse (str (:text b)))
          entries (when (some-> parsed :total pos?)
                    (resolve-locations root (failure-entries parsed)))]
      (fn [s _]
        (if entries
          (show-results s parsed root entries)
          (cmd/msg s (if (= 0 (:job-exit b))
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
      (job/start s {:argv ["sh" "-c" cmd] :dir root :buf-name name
                    :on-finish (partial finish-report root)}))))
