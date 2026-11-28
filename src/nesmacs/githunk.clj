(ns nesmacs.githunk
  "Hunk-level staging for the current file. `s` stages the hunk at point,
   `u` unstages it, `g` refreshes. Uses `git apply --cached` on the hunk's
   patch text, so no interactive git process is needed.

   Opening the listing is a job; staging/unstaging and the refresh afterward
   are short `git apply`/`git diff` calls."
  (:require [clojure.string :as str]
            [nesmacs.cmd :as cmd]
            [nesmacs.deferred :as deferred]
            [nesmacs.git :as git]
            [nesmacs.listbuf :as listbuf]))

(def ^:private hunks-buf "*Hunks*")

(defn- fmt [e]
  (format "%s %s" (if (:staged e) "staged" "worktree") (:label e)))

(defn- spec [dir path staged? entries]
  {:name hunks-buf
   :mode :hunks-mode
   :header (str (if staged? "Staged" "Unstaged") " hunks for " path
                "   s=stage u=unstage g=refresh")
   :entries (mapv #(assoc % :path path :staged staged?) entries)
   :format-entry fmt
   :extras {:git-dir dir :hunk-path path :hunk-staged? staged?}})

(defn open
  ([s] (open s false))
  ([s staged?]
   (let [b (cmd/cur s)
         path (:file b)
         dir (git/current-dir b)]
     (if path
       (if-let [root (git/repo-root dir)]
         (deferred/start s
                         {:argv (git/diff-argv (git/path-arg root path) staged?)
                          :dir root
                          :job-name "*git-diff*"
                          :build (fn [out]
                                   (if-let [entries (seq (git/parse-hunks out))]
                                     [hunks-buf (spec root path staged? entries)]
                                     (if staged? "No staged hunks" "No unstaged hunks")))})
         (cmd/msg s "Not a git repository"))
       (cmd/msg s "Buffer has no file")))))

(defn refresh [s]
  (let [{dir :git-dir path :hunk-path staged? :hunk-staged?} (cmd/cur s)
        entries ((if staged? git/staged-hunks git/diff-hunks) dir path)]
    (listbuf/refresh s hunks-buf (spec dir path staged? entries))))

(defn- alter-hunk
  [s reverse? label]
  (if-let [e (listbuf/entry-at-point s)]
    (let [out (git/apply-cached (:git-dir (cmd/cur s)) (:patch e) reverse?)]
      (if (or (str/blank? out) (not (re-find #"(?i)error|fatal|patch failed" out)))
        (-> s refresh (cmd/msg (str label " hunk " (:index e))))
        (cmd/msg s (str "git apply: " (first (str/split-lines out))))))
    s))
  ; shortcut: `git apply --cached` runs synchronously on the editor loop.
  ; A huge patch (many MB) would stall the UI; acceptable for typical hunk sizes.

(defn stage   [s] (alter-hunk s false "Staged"))
(defn unstage [s] (alter-hunk s true "Unstaged"))

(def hunks-keymap
  {\s stage
   \u unstage
   \g refresh})
