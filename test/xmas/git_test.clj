(ns xmas.git-test
  (:require [clojure.test :refer [deftest is]]
            [xmas.git :as git]))

(deftest current-dir-precedence
  (is (= "/tmp/explicit" (git/current-dir {:git-dir "/tmp/explicit"
                                           :job-dir "/tmp/job"
                                           :file "/tmp/a/file"})))
  (is (= "/tmp/job" (git/current-dir {:job-dir "/tmp/job"
                                      :file "/tmp/a/file"})))
  (is (= "/tmp/a" (git/current-dir {:file "/tmp/a/file"})))
  (is (= (System/getProperty "user.dir") (git/current-dir {}))))