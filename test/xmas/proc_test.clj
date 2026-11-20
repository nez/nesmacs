(ns xmas.proc-test
  (:require [clojure.test :refer [deftest is]]
            [xmas.proc :as proc]))

(defn- collect-until-finished
  "Poll `drain!` like the editor loop does. Returns [handle text code]."
  [h]
  (loop [text "" i 0]
    (let [chunk (proc/drain! h 1000000)
          text  (str text chunk)]
      (cond
        (proc/finished? h) [h text (proc/exit-code h)]
        (> i 500)          [h text :timeout]
        :else              (do (Thread/sleep 10) (recur text (inc i)))))))

(deftest streams-output-and-reports-exit
  (let [h (proc/start {:argv ["sh" "-c" "printf 'hi\\nthere\\n'"]})
        [_ text code] (collect-until-finished h)]
    (is (= "hi\nthere\n" text))
    (is (= 0 code))))

(deftest nonzero-exit-is-reported
  (let [h (proc/start {:argv ["sh" "-c" "exit 3"]})
        [_ _ code] (collect-until-finished h)]
    (is (= 3 code))))

(deftest merges-stderr-into-output
  ;; M2: a process writing to stderr must not fill the pipe or disappear.
  (let [h (proc/start {:argv ["sh" "-c" "printf 'out\\n'; printf 'err\\n' >&2"]})
        [_ text code] (collect-until-finished h)]
    (is (= 0 code))
    (is (.contains ^String text "out"))
    (is (.contains ^String text "err"))))

(deftest drain-yields-output-in-chunks
  ;; The limit is a soft per-tick cap: a chunk is never split, so one drain can
  ;; carry up to the reader's 8 KiB buffer.
  (let [h (proc/start {:argv ["sh" "-c" "head -c 20000 /dev/zero | tr '\\0' x"]})
        chunks (loop [acc [] i 0]
                 (let [acc (conj acc (proc/drain! h 100))]
                   (if (or (proc/finished? h) (> i 500))
                     acc
                     (do (Thread/sleep 5) (recur acc (inc i))))))]
    (is (every? #(<= (count %) 8192) chunks))
    (is (= 20000 (reduce + (map count chunks))))))

(deftest runs-in-given-directory
  (let [dir (System/getProperty "java.io.tmpdir")
        h (proc/start {:argv ["pwd"] :dir dir})
        [_ text _] (collect-until-finished h)]
    (is (= (.getCanonicalPath (java.io.File. dir))
           (clojure.string/trim text)))))

(deftest kill-stops-a-long-process
  (let [h (proc/start {:argv ["sleep" "30"]})]
    (is (some? (proc/by-id (:id h))))
    (proc/kill! h)
    (is (nil? (proc/by-id (:id h))))
    (loop [i 0]
      (when (and (nil? (proc/exit-code h)) (< i 500))
        (Thread/sleep 10)
        (recur (inc i))))
    (is (some? (proc/exit-code h)))
    (is (not (.isAlive ^Process (:process h))))))