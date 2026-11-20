(ns xmas.keymap
  "Key-string parsing and keymap installation.

   A keymap is a nested map: a key (char, keyword like :return, or a
   [:ctrl \\x] vector) maps either to a command `state -> state` or to another
   map, which makes it a prefix."
  (:require [clojure.string :as str]))

(defn- err [msg] (throw (ex-info msg {:type ::key-error})))

(def ^:private named-keys
  {"RET" :return "TAB" :tab "DEL" :backspace "ESC" :escape "SPC" \space
   "<up>" :up "<down>" :down "<left>" :left "<right>" :right
   "<home>" :home "<end>" :end "<prior>" :page-up "<next>" :page-down})

(defn- key-name [token]
  (or (get named-keys token)
      (when (= 1 (count token)) (.charAt ^String token 0))
      (err (str "Unknown key: " token))))

(defn- parse-key-token [token]
  (cond
    (.startsWith ^String token "C-")
    (let [key (key-name (subs token 2))]
      (if (char? key) [:ctrl key] (err (str "Invalid control key: " token))))

    (.startsWith ^String token "M-")
    (let [key (key-name (subs token 2))]
      (if (char? key) [:meta key] (err (str "Invalid meta key: " token))))

    :else (key-name token)))

(defn- parse-standard-token [token]
  (if (or (.startsWith ^String token "C-")
          (.startsWith ^String token "M-")
          (contains? named-keys token)
          (= 1 (count token)))
    [(parse-key-token token)]
    (vec token)))

(def ^:private legacy-key-token-re #"\\C-(.)|\\M-(.)|\\(.)|(.)")

(defn parse-key-string
  "Parse standard Emacs key syntax such as \"C-x C-s\" into a key path.
   The legacy \"\\C-x\\C-s\" spelling remains accepted."
  [^String s]
  (if (.contains s "\\")
    (mapv (fn [[_ c m lit plain]]
            (let [ch (.charAt ^String (or c m lit plain) 0)]
              (cond c [:ctrl ch], m [:meta ch], :else ch)))
          (re-seq legacy-key-token-re s))
    (let [s (str/trim s)]
      (if (empty? s) [] (vec (mapcat parse-standard-token (str/split s #"\s+")))))))

(defn bind
  "Install `handler` at `path ++ keys` in `state`. A prefix already bound to a
   command is replaced by a map, so binding \"C-x C-f\" after \"C-x\" was a
   plain command does not blow up."
  [state path keys handler]
  (let [full (into path keys)]
    (loop [s state prefix path remaining keys]
      (cond
        (empty? remaining) (assoc-in s full handler)
        :else (let [cur (get-in s prefix)
                    s'  (if (or (nil? cur) (map? cur))
                          s
                          (assoc-in s prefix {}))]
                (recur s' (conj prefix (first remaining)) (rest remaining)))))))