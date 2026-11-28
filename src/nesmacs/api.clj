(ns nesmacs.api
  "The extension surface used by `~/.nesmacs/init.clj` and by `M-:`.

   Every mutation swaps the atom in `*state*`. Its root is the live editor
   atom under `-main` (so nREPL callers need no wrapping). `nesmacs.ed` binds it
   to a per-command atom during `M-:`, so the same calls are safe inside and
   outside the command loop's transaction."
  (:require [nesmacs.buf :as buf]
            [nesmacs.cmd :as cmd]
            [nesmacs.complete :as complete]
            [nesmacs.keymap :as keymap]
            [nesmacs.lang :as lang]
            [nesmacs.mode :as mode]
            [nesmacs.persist :as persist]
            [nesmacs.props :as props]
            [nesmacs.view :as view]))

(def ^:dynamic *state* nil)

(defn- target []
  (or *state*
      (throw (IllegalStateException.
               "nesmacs.api used outside a config or M-: context: nesmacs.api/*state* is unbound"))))

(defn- handler
  "Capture a Var, not its current value, so redefining it later updates every
   key and hook already pointing at it."
  [f] (if (var? f) (bound-fn* f) f))

(defn- register-command
  "Record a named command for M-x. `src` is the Var the user passed (for its
   name and docstring); `f` is the handler actually installed. An existing
   entry keeps its docstring — binding a builtin must not blank its help."
  [s src f]
  (if-let [{:keys [name doc]} (meta src)]
    (let [n (str name)]
      (if (contains? (:commands s) n)
        (assoc-in s [:commands n :fn] f)
        (assoc-in s [:commands n] {:fn f :doc doc})))
    s))

(defn apply!
  "Apply a state transition inside the current config / M-: transaction.
   Returns nil so M-: echoes nothing instead of the whole editor state."
  [f & args]
  (apply swap! (target) f args)
  nil)

(defn after-command
  "Queue an effect for the editor loop after the command commits. The effect
   receives a function that applies a state transition to the live editor."
  [s effect]
  (update s ::effects (fnil conj []) effect))

(defn persist-setting [s k]
  (let [value (get s k)]
    (after-command s (fn [_] (persist/save-quiet! (name k) value)))))

(defn run-effects!
  "Drain committed effects on the editor thread, outside the command swap."
  [editor]
  (when (seq (::effects @editor))
    (let [[before] (swap-vals! editor dissoc ::effects)
          publish! (fn [f & args] (apply swap! editor f args))]
      (doseq [effect (::effects before)]
        (try (effect publish!)
             (catch Exception e (publish! cmd/msg-error "Extension effect failed" e)))))))

(defn- install [s path key f]
  (let [cmd (handler f)]
    (-> s (keymap/bind path (keymap/parse-key-string key) cmd)
          (register-command f cmd))))

(defn bind!
  "Bind a key string (e.g. \"C-c g\") to a command fn or Var. Named Vars also
   become visible to M-x."
  [key f]
  (apply! install [:bindings] key f))

(defn register-command!
  "Expose a named command Var in M-x without assigning a key."
  [command]
  (when-not (var? command)
    (throw (IllegalArgumentException. "register-command! expects a Var")))
  (apply! register-command command (handler command)))

(defn add-hook!
  "Append `f` to a hook. Accepts `:clojure-mode-hook` or `'clojure-mode-hook`."
  [hook f]
  (apply! mode/add-hook hook (handler f)))

(defn configure-lsp!
  "Set the server argv globally or for one language id."
  ([argv] (apply! assoc :lsp-command (mapv str argv)))
  ([language argv] (apply! assoc-in [:lsp-commands (name language)] (mapv str argv))))

(defn register-language!
  "Register a language descriptor with a mode, set of extensions and optional tools."
  [id {:keys [mode exts lsp] :as spec}]
  (when-not (and (simple-keyword? id) (simple-keyword? mode) (set? exts) (seq exts)
                 (every? string? exts))
    (throw (IllegalArgumentException. "Expected a language id, mode and set of extensions")))
  (lang/register! id spec)
  (apply! mode/register mode :major)
  (when lsp
    (apply! assoc-in [:completion-sources mode]
            [complete/buffer-symbols complete/project-symbols
             (requiring-resolve 'nesmacs.ed/lsp-completion-source)]))
  nil)

(defn configure-agent!
  "Set the argv opened by `M-x agent` (the AI CLI lives in a terminal buffer;
   the editor has no API client)."
  [argv]
  (apply! assoc :agent-command (mapv str argv)))

(defn register-project-task!
  "Register a task for a project root. The task has the same shape as .nesmacs.edn."
  [root task spec]
  (when-not (and (simple-keyword? task) (map? spec))
    (throw (IllegalArgumentException. "Expected a task keyword and a task map")))
  (apply! assoc-in [:project-task-overrides (.getCanonicalPath (java.io.File. (str root))) task] spec))

(defn use-theme!
  "Switch to a named palette (:default or :light). The editor loop applies it,
   so this also works from init.clj and persists with the session."
  [theme]
  (apply! assoc :theme (keyword theme)))

(defn set-face!
  "Merge {:fg n :bg n :bold bool} into one face for ad-hoc tweaks. A later
   `use-theme!` replaces the whole palette, so prefer a theme for broad changes."
  [face attrs]
  (view/set-face-attr! (keyword face) attrs))

(defn- update-props!
  "Clamp the range to the current buffer, then replace :props with
   `(f props from to)`."
  [f from to]
  (apply! cmd/update-cur
          (fn [b]
            (let [[from to] (buf/clamp-range b from to)]
              (assoc b :props (f (:props b) from to))))))

(defn add-prop!
  "Paint the current buffer range [from to) with `face`, adding to whatever
   is already there. `opts` may carry :owner (so a later remove-prop! can drop
   it), :priority, and any extra data. Props follow edits until the text they
   cover disappears.

   Absolute offsets are deliberate: hooks know the buffer, and there is no
   stable line identity in a plain text buffer. The range is clamped to the
   buffer, like every other edit entry point."
  ([from to face] (add-prop! from to face {}))
  ([from to face opts]
   (update-props! (fn [ps from to]
                    (props/put ps (props/make from to face opts)))
                  from to)))

(defn set-prop!
  "Replace every prop owned by `owner` in the current buffer with one range.
   Removes the old ranges first, so a moving match or hover target does not
   leave stale paint behind."
  ([owner from to face] (set-prop! owner from to face {}))
  ([owner from to face opts]
   (update-props! (fn [ps from to]
                    (props/set-owner ps owner from to face opts))
                  from to)))

(defn remove-prop!
  "Remove every prop owned by `owner` from the current buffer."
  [owner]
  (apply! cmd/update-cur #(assoc % :props (props/remove-owner (:props %) owner))))

(defn register-mode!
  "Register a major or minor mode. `keymap` maps key strings to commands, e.g.
   {\"C-c C-c\" #'run}."
  [name type keymap]
  (apply! (fn [s]
            (reduce-kv (fn [s k f] (install s [:modes name :keymap] k f))
                       (mode/register s name type)
                       keymap))))
