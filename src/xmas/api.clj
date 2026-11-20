(ns xmas.api
  "The extension surface used by `~/.xmas/init.clj` and by `M-:`.

   Every mutation swaps the atom in `*state*`. Its root is the live editor
   atom under `-main` (so nREPL callers need no wrapping). `xmas.ed` binds it
   to a per-command atom during `M-:`, so the same calls are safe inside and
   outside the command loop's transaction."
  (:require [xmas.keymap :as keymap]
            [xmas.mode :as mode]))

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

(defn register-mode!
  "Register a major or minor mode. `keymap` maps key strings to commands, e.g.
   {\"C-c C-c\" #'run}."
  [name type keymap]
  (apply! (fn [s]
            (reduce-kv (fn [s k f] (install s [:modes name :keymap] k f))
                       (mode/register s name type)
                       keymap))))