(ns xmas.gitblame
  "Read-only blame listing for one file. RET jumps to the line in the
   source buffer."
  (:require [xmas.cmd :as cmd]
            [xmas.git :as git]
            [xmas.listbuf :as listbuf]))

(defn- fmt [{:keys [hash author date text]}]
  (format "%s %s %-12s %s" hash date (or author "") text))

(defn- spec [buf dir path]
  (let [entries (mapv #(assoc % :buf (:name buf) :path path)
                      (git/blame dir path))]
    {:name (str "*Blame " (:name buf) "*")
     :mode :nav-mode
     :header (str "Blame " path " (" (count entries) " lines)")
     :entries entries
     :format-entry fmt
     :extras {:nav-root nil}}))

(defn open [s]
  (let [b (cmd/cur s)
        path (:file b)
        dir (git/current-dir b)]
    (cond
      (nil? path) (cmd/msg s "Buffer has no file")
      (not (git/is-repo? dir)) (cmd/msg s "Not a git repository")
      :else
      (let [sp (spec b dir path)]
        (if (empty? (:entries sp))
          (cmd/msg s "No blame (untracked or empty file)")
          (-> s (assoc-in [:bufs (:name sp)] (listbuf/make sp))
                (cmd/set-cur-buffer (:name sp))))))))