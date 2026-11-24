(ns xmas.githunk
  "Hunk-level staging for the current file. `s` stages the hunk at point,
   `u` unstages it, `g` refreshes. Uses `git apply --cached` on the hunk's
   patch text, so no interactive git process is needed."
  (:require [clojure.string :as str]
            [xmas.cmd :as cmd]
            [xmas.git :as git]
            [xmas.listbuf :as listbuf]))

(def ^:private hunks-buf "*Hunks*")

(defn- fmt [e]
  (format "%s %s" (if (:staged e) "staged" "worktree") (:label e)))

(defn- spec [dir path staged?]
  (let [entries (mapv #(assoc % :path path :staged staged?)
                      ((if staged? git/staged-hunks git/diff-hunks) dir path))]
    {:name hunks-buf
     :mode :hunks-mode
     :header (str (if staged? "Staged" "Unstaged") " hunks for " path
                  "   s=stage u=unstage g=refresh")
     :entries entries
     :format-entry fmt
     :extras {:git-dir dir :hunk-path path :hunk-staged? staged?}}))

(defn open
  ([s] (open s false))
  ([s staged?]
   (let [b (cmd/cur s)
         path (:file b)
         dir (git/current-dir b)]
     (cond
       (nil? path) (cmd/msg s "Buffer has no file")
       (not (git/is-repo? dir)) (cmd/msg s "Not a git repository")
       :else (listbuf/open s hunks-buf (spec dir path staged?))))))

(defn- alter-hunk
  [s reverse? label]
  (if-let [e (listbuf/entry-at-point s)]
    (let [b (cmd/cur s)
          dir (:git-dir b)
          out (git/apply-cached dir (:patch e) reverse?)]
      (if (or (str/blank? out) (not (re-find #"(?i)error|fatal|patch failed" out)))
        (-> s
            (listbuf/refresh hunks-buf (spec dir (:path e) (:hunk-staged? b)))
            (cmd/msg (str label " hunk " (:index e))))
        (cmd/msg s (str "git apply: " (first (str/split-lines out))))))
    s))

(defn stage   [s] (alter-hunk s false "Staged"))
(defn unstage [s] (alter-hunk s true "Unstaged"))

(defn refresh [s]
  (let [b (cmd/cur s)]
    (listbuf/refresh s hunks-buf (spec (:git-dir b) (:hunk-path b) (:hunk-staged? b)))))

(def hunks-keymap
  {\s stage
   \u unstage
   \g refresh})