(ns nesmacs.packages-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [nesmacs.cmd :as cmd]
            [nesmacs.ed :as ed]
            [nesmacs.packagehub :as hub]
            [nesmacs.packages :as packages]
            [nesmacs.persist :as persist]
            [nesmacs.testutil :as tu])
  (:import [java.nio.file Files]
           [java.security MessageDigest]
           [java.util HexFormat]
           [java.util.jar JarEntry JarOutputStream]))

(use-fixtures :each
  (fn [f]
    (let [dir (tu/temp-dir "nesmacs-packages")]
      (try (binding [persist/*dir* dir]
             (with-redefs-fn {#'packages/bundled-packages (constantly {})} f))
           (finally (tu/delete-tree dir))))))

(def ^:private id :fixture/notes)

(defn- fixture-release! [version fail? & [package-id]]
  (let [id (or package-id id)
        slug (str (gensym "plugin"))
        ns (symbol (str "fixture." slug))
        manifest {:id id :version version :api 1 :entry (symbol (str ns) "start!")
                  :options [{:key :label :label "Notes label" :type :string :default "Default notes"}]}
        source (str "(ns " ns " (:require [nesmacs.api :as api]))"
                    "(def ^:private label (atom nil))"
                    "(defn package-probe \"Fixture command.\" [s] (assoc s :msg @label))"
                    "(defn start! [options] "
                    "  (reset! label (:label options))"
                    "  (api/register-command! #'package-probe)"
                    (when fail? " (throw (ex-info \"fixture failure\" {}))")
                    ")")
        file (io/file (persist/directory) (str slug "-" version ".jar"))]
    (with-open [jar (JarOutputStream. (io/output-stream file))]
      (doseq [[path content] [["plugin.edn" (pr-str manifest)]
                              [(str "fixture/" slug ".clj") source]]]
        (.putNextEntry jar (JarEntry. path))
        (.write jar (.getBytes ^String content "UTF-8"))
        (.closeEntry jar)))
    {:id id :version version :api 1 :name "Notes fixture"
     :description "Exercise a real downloadable JAR"
     :url (.getName file)
     :sha256 (.formatHex (HexFormat/of)
                         (.digest (MessageDigest/getInstance "SHA-256")
                                  (Files/readAllBytes (.toPath file))))}))

(defn- catalog! [& releases]
  (let [file (io/file (persist/directory) "catalog.edn")]
    (spit file (pr-str {:format 1 :packages (vec releases)}))
    (.getPath file)))

(defn- refresh! [source]
  (packages/execute! {:action :catalog :source source}))

(defn- install! [release]
  (packages/execute! {:action :install :id (:id release) :checksum (:sha256 release)}))

(defn- installed [store]
  (get-in store [:installed id :release :version]))

(deftest install-configure-and-disable
  (let [release (fixture-release! "1.0" false)]
    (refresh! (catalog! release))
    (is (= "1.0" (installed (install! release))))
    (packages/execute! {:action :configure :id id :option :label :value "My notes"})
    (let [state (packages/initialize! {})
          command (get-in state [:commands "package-probe" :fn])]
      (is (= "My notes" (:msg (command state))))
      (is (true? (get-in state [:packages :store :installed id :confirmed?]))))
    (packages/execute! {:action :disable :id id})
    (is (empty? (:commands (packages/initialize! {}))))
    (is (true? (get-in (packages/execute! {:action :enable :id id})
                       [:installed id :enabled?])))
    (is (nil? (installed (packages/execute! {:action :remove :id id}))))))

(deftest failed-update-retains-last-working-version
  (let [release (fixture-release! "1.0" false)
        upgrade (fixture-release! "2.0" true)]
    (refresh! (catalog! release))
    (install! release)
    (is (get-in (packages/initialize! {}) [:commands "package-probe"]))
    (refresh! (catalog! upgrade))
    (is (= "2.0" (installed (install! upgrade))))
    (let [state (packages/initialize! {})]
      (is (= "1.0" (installed (get-in state [:packages :store]))))
      (is (re-find #"fixture failure.*Previous version selected"
                   (get-in state [:packages :errors id])))
      (is (nil? (get-in state [:commands "package-probe"]))
          "A failed entrypoint's registrations never reach the live editor"))))

(deftest invalid-catalog-and-download-leave-committed-state-intact
  (let [release (fixture-release! "1.0" false)
        bad (assoc release :sha256 (apply str (repeat 64 "0")))
        store (refresh! (catalog! bad))]
    (testing "a bad checksum is rejected before installation"
      (is (thrown-with-msg? Exception #"checksum does not match" (install! bad)))
      (is (empty? (get-in (packages/initialize! {}) [:packages :store :installed]))))
    (testing "failed refresh leaves the previous catalog usable offline"
      (is (thrown-with-msg? Exception #"Invalid extension catalog"
                            (refresh! (catalog! release release))))
      (is (= store (get-in (packages/initialize! {}) [:packages :store]))))))

(defn- current-text [state]
  (str (:text (cmd/cur state))))

(defn- finish-operation! [state]
  (let [editor (atom state)]
    (loop [ticks 0]
      (hub/tick! editor)
      (if (or (get-in @editor [:packages :operation]) (get-in @editor [:packages :job]))
        (do
          (when (>= ticks 1000) (throw (ex-info "Hub operation timed out" {})))
          (Thread/sleep 5)
          (recur (inc ticks)))
        @editor))))

(deftest keyboard-hub-uses-editor-commands
  (let [release (fixture-release! "1.0" false)
        _ (refresh! (catalog! release))
        start (packages/initialize! (#'ed/initial-state 24 100))
        open-hub (get-in start [:commands "hub" :fn])
        hub (finish-operation! (open-hub start))
        detail (ed/run-key hub :return)
        installing (ed/run-key detail :return)
        saved (finish-operation! installing)]
    (is (re-find #"\[Explore\].*Installed" (current-text hub)))
    (is (re-find #"Install 1.0" (current-text detail)))
    (is (= "1.0" (installed (get-in saved [:packages :store]))))
    (is (re-find #"Restart to activate" (current-text saved)))
    (let [installed-tab (ed/run-key saved :tab)
          back (ed/run-key installed-tab \q)]
      (is (re-find #"\[Installed\]" (current-text installed-tab)))
      (is (= "*scratch*" (:buf back))))))

(deftest update-all-refreshes-and-stages-each-compatible-installed-package
  (let [a (fixture-release! "1.0" false :fixture/a)
        b (fixture-release! "1.0" false :fixture/b)
        c (fixture-release! "1.0" false :fixture/c)
        d (fixture-release! "1.0" false :fixture/d)
        _ (refresh! (catalog! a b c d))]
    (doseq [release [a b c d]] (install! release))
    (let [started (packages/initialize! (#'ed/initial-state 24 100))
          _ (packages/execute! {:action :disable :id :fixture/b})
          _ (packages/execute! {:action :configure :id :fixture/b :option :label :value "Keep me"})
          hub (finish-operation! (hub/open started))
          upgrade-a (fixture-release! "2.0" false :fixture/a)
          upgrade-b (fixture-release! "2.0" false :fixture/b)
          upgrade-c (assoc (fixture-release! "2.0" false :fixture/c) :sha256 (apply str (repeat 64 "0")))
          incompatible (assoc (fixture-release! "2.0" false :fixture/d) :api 999)
          available (fixture-release! "1.0" false :fixture/new)
          _ (catalog! upgrade-a upgrade-b upgrade-c incompatible available)
          result (finish-operation! (ed/run-key hub \U))
          records (get-in result [:packages :store :installed])
          version #(get-in records [% :release :version])]
      (is (= "2.0" (version :fixture/a)))
      (is (= "2.0" (version :fixture/b)))
      (is (= "1.0" (get-in records [:fixture/a :previous :release :version])))
      (is (false? (get-in records [:fixture/b :enabled?])))
      (is (= "Keep me" (get-in records [:fixture/b :options :label])))
      (is (= "1.0" (version :fixture/c)))
      (is (= "1.0" (version :fixture/d)))
      (is (nil? (version :fixture/new)))
      (is (re-find #"checksum does not match" (get-in result [:packages :update-errors :fixture/c])))
      (is (re-find #"2 updated, 1 failed" (:msg result)))
      (is (get-in result [:commands "package-upgrade-all" :fn]))
      (is (get-in result [:commands "package-update-all" :fn])))))

(deftest update-all-uses-the-last-good-catalog-offline
  (let [release (fixture-release! "1.0" false)
        upgrade (fixture-release! "2.0" false)]
    (refresh! (catalog! release))
    (install! release)
    (let [source (catalog! upgrade)]
      (refresh! source)
      (io/delete-file source)
      (let [{:keys [catalog-error store updated failed]} (packages/update-all! nil)]
        (is (seq catalog-error))
        (is (= [id] updated))
        (is (empty? failed))
        (is (= "2.0" (installed store)))))))
