(ns xmas.api
  "The extension surface used by `~/.xmas/init.clj` and by `M-:`.

   Every mutation swaps the atom in `*state*`. Its root is the live editor
   atom under `-main` (so nREPL callers need no wrapping). `xmas.ed` binds it
   to a per-command atom during `M-:`, so the same calls are safe inside and
   outside the command loop's transaction."
  (:require [xmas.cmd :as cmd]
            [xmas.keymap :as keymap]
            [xmas.mode :as mode]
            [xmas.props :as props]
            [xmas.view :as view]))

(def ^:dynamic *state* nil)

(defn- target []
  (or *state*
      (throw (IllegalStateException.
               "xmas.api used outside a config or M-: context: xmas.api/*state* is unbound"))))

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

(defn- install [s path key f]
  (let [cmd (handler f)]
    (-> s (keymap/bind path (keymap/parse-key-string key) cmd)
          (register-command f cmd))))

(defn bind!
  "Bind a key string (e.g. \"C-c g\") to a command fn or Var. Named Vars also
   become visible to M-x."
  [key f]
  (apply! install [:bindings] key f))

(defn add-hook!
  "Append `f` to a hook. Accepts `:clojure-mode-hook` or `'clojure-mode-hook`."
  [hook f]
  (apply! mode/add-hook hook (handler f)))

(defn configure-lsp!
  "Set the argv used to start language servers from now on."
  [argv]
  (apply! assoc :lsp-command (mapv str argv)))

(defn configure-agent!
  "Set the argv opened by `M-x agent` (the AI CLI lives in a terminal buffer;
   the editor has no API client)."
  [argv]
  (apply! assoc :agent-command (mapv str argv)))

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

(defn- clamp-range
  "Clamp [from to) to the buffer text, the way buf/edit does."
  [b from to]
  (let [n    (count (:text b))
        from (-> (long from) (max 0) (min n))]
    [from (-> (long to) (max from) (min n))]))

(defn- update-props!
  "Clamp the range to the current buffer, then replace :props with
   `(f props from to)`."
  [f from to]
  (apply! cmd/update-cur
          (fn [b]
            (let [[from to] (clamp-range b from to)]
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