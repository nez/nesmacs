(ns xmas.mode
  "Major and minor mode registry. Lives as data on the editor state under
   :modes {name {:type :major|:minor :keymap {...}}} and :hooks {name [fns]}.
   File-extension -> mode comes from the language registry in `xmas.lang`."
  (:require [xmas.cmd :as cmd]
            [xmas.lang :as lang]))

;; --- Registration ---

(defn register
  [s mode-name type & {:keys [keymap]}]
  (-> s
      (assoc-in [:modes mode-name :type] type)
      (update-in [:modes mode-name :keymap] #(merge (or % {}) keymap))))

;; --- Hooks ---

;; Accept :my-mode-hook, "my-mode-hook" and 'my-mode-hook as the same hook.
(defn- hook-key [h] (if (or (keyword? h) (string? h)) (symbol (name h)) h))

(defn add-hook [s hook-name f]
  (update-in s [:hooks (hook-key hook-name)] (fnil conj []) f))

(defn run-hooks [s hook-name]
  (reduce (fn [s f] (f s)) s (get-in s [:hooks (hook-key hook-name)] [])))

;; --- Activation ---

(defn- hook-name-for [mode-name]
  (symbol (str (name mode-name) "-hook")))

(defn set-major-mode [s mode-name]
  (-> s
      (cmd/update-cur #(assoc % :mode mode-name
                              :font-lock [] :font-lock-window nil))
      (run-hooks (hook-name-for mode-name))))

(defn toggle-minor-mode [s mode-name]
  (let [active (vec (:minor-modes (cmd/cur s)))
        active? (some #{mode-name} active)
        next-active (if active? (filterv #(not= mode-name %) active)
                                (conj active mode-name))]
    (-> s
        (cmd/update-cur #(assoc % :minor-modes next-active))
        (run-hooks (hook-name-for mode-name)))))

;; --- Key lookup ---
;; Active minor modes override the major mode, which overrides global.

(defn lookup-key [s key]
  (let [b (cmd/cur s)]
    (or (some (fn [m] (get-in s [:modes m :keymap key]))
              (rseq (vec (:minor-modes b))))
        (get-in s [:modes (:mode b) :keymap key]))))

;; --- File extension → mode ---

(defn mode-for-file
  "Return the major-mode keyword matching the filename, or nil."
  [^String filename]
  (lang/mode-for-file filename))
