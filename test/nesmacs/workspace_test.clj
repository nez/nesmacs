(ns nesmacs.workspace-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [nesmacs.api :as api]
            [nesmacs.ed :as ed]
            [nesmacs.job :as job]
            [nesmacs.nav :as nav]
            [nesmacs.persist :as persist]
            [nesmacs.project :as project]
            [nesmacs.repl :as repl]
            [nesmacs.session :as session]
            [nesmacs.testutil :as tu]
            [nesmacs.workspace :as workspace]
            [nrepl.server :as nrepl]))

(defn- checkout [parent dir]
  (let [root (io/file parent dir)]
    (.mkdir root)
    (spit (io/file root "src.txt") dir)
    (spit (io/file root ".nesmacs.edn")
          (pr-str {:tasks {:test {:argv ["sh" "-c" "printf 'src.txt:1: error: failing test\\n'; exit 1"]
                                  :report :tests}}}))
    (.getCanonicalPath root)))

(deftest task-results-keep-their-project-after-switching
  (let [dir (tu/temp-dir "nesmacs-workspace")]
    (try
      (binding [persist/*dir* dir]
        (let [a (checkout dir "alpha") b (checkout dir "beta")
              editor (atom (-> (#'ed/initial-state 24 80)
                               (workspace/activate a) (workspace/run-task a :test)
                               (workspace/activate b) (workspace/run-task b :test) workspace/menu))
              dashboard (:buf @editor)
              names (map #(workspace/buffer-name % "Results test") [a b])]
          (api/run-effects! editor)
          (is (loop [attempts 0]
                (swap! editor job/pump!)
                (job/consume-results! editor)
                (cond
                  (every? #(get-in @editor [:bufs % :listbuf-entries]) names) true
                  (= attempts 400) false
                  :else (do (Thread/sleep 10) (recur (inc attempts))))))
          (is (= dashboard (:buf @editor)))
          (doseq [[root result] (map vector [a b] names)]
            (let [s (ed/switch-buffer @editor result)
                  s (ed/handle-key s [:ctrl \n])
                  jumped (nav/jump-at-point s)]
              (is (= root (project/context s)))
              (is (= (.getCanonicalPath (io/file root "src.txt")) (:file (ed/cur jumped))))))))
      (finally (tu/delete-tree dir)))))

(deftest switching-projects-restores-their-panes-after-restart
  (let [dir (tu/temp-dir "nesmacs-workspace-session")]
    (try
      (binding [persist/*dir* dir]
        (let [a (checkout dir "alpha") b (checkout dir "beta")
              base (#'ed/initial-state 24 80)
              s (-> base (workspace/activate a)
                    (ed/find-file (.getPath (io/file a "src.txt"))) ed/split-window-right
                    (workspace/activate b))]
          (session/save-session! s)
          (let [[restored] (session/restore-session base (session/load-session!))
                switched (workspace/activate restored a)]
            (is (= b (project/context restored)))
            (is (= :split (get-in switched [:windows :type])))
            (is (= a (project/context switched))))))
      (finally (tu/delete-tree dir)))))

(deftest project-evaluation-uses-its-own-nrepl
  (let [dir (tu/temp-dir "nesmacs-project-repl")]
    (with-open [server (nrepl/start-server :bind "127.0.0.1" :port 0)]
      (try
        (binding [persist/*dir* dir]
          (let [root (checkout dir "repl")
                name (workspace/buffer-name root "REPL")
                editor (atom (-> (#'ed/initial-state 24 80)
                                 (workspace/activate root)
                                 (assoc-in [:project-repl-ports root] (:port server))
                                 (ed/switch-buffer "demo.clj")
                                 (ed/edit 0 0 "(+ 2 3)\n(* 2 3)")
                                 (ed/handle-key [:meta \<])
                                 (repl/evaluate :form)))]
            (api/run-effects! editor)
            (is (loop [attempts 0]
                  (cond
                    (= "5\n" (str (get-in @editor [:bufs name :text]))) true
                    (= attempts 600) false
                    :else (do (Thread/sleep 10) (recur (inc attempts))))))))
        (finally (repl/close-all!) (tu/delete-tree dir))))))
