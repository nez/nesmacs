(ns nesmacs.register
  "Single-character registers for points, regions, and window configurations."
  (:require [nesmacs.cmd :as cmd]
            [nesmacs.gap :as gap]
            [nesmacs.window :as win]))

(def ^:private prompts
  {:reg-point "Point to register: "
   :reg-jump "Jump to register: "
   :reg-copy "Copy to register: "
   :reg-insert "Insert register: "
   :reg-window "Window configuration to register: "})

(defn- prompt [s tag]
  (-> s (assoc :capture-next tag) (cmd/msg (prompts tag))))

(defn point-to-register [s] (prompt s :reg-point))
(defn jump-to-register [s] (prompt s :reg-jump))
(defn copy-to-register [s] (prompt s :reg-copy))
(defn insert-register [s] (prompt s :reg-insert))
(defn window-configuration-to-register [s] (prompt s :reg-window))

(defn capturing? [s]
  (contains? prompts (:capture-next s)))

(defn- restore-point [s {:keys [buf pos]}]
  (if (contains? (:bufs s) buf)
    (-> s (cmd/set-cur-buffer buf) (cmd/goto-char pos))
    (cmd/msg s (str "Buffer gone: " buf))))

(defn- restore-windows [s {:keys [windows cur-window]}]
  (if (every? #(contains? (:bufs s) (:buffer (get-in windows %))) (win/leaves windows))
    (-> s
        (assoc :windows windows :cur-window cur-window
               :buf (get-in windows (conj cur-window :buffer)))
        cmd/load-window-point)
    (cmd/msg s "Window configuration references a killed buffer")))

(defn- use-register [s key tag]
  (let [r (get-in s [:registers key])]
    (cond
      (nil? r) (cmd/msg s (str "Register " key " is empty"))
      (= tag :reg-insert)
      (if (= :text (:kind r))
        (cmd/insert-at-point s (:text r))
        (cmd/msg s (str "Register " key " is not text")))
      :else
      (case (:kind r)
        :point (restore-point s r)
        :window (restore-windows s r)
        (cmd/msg s (str "Register " key " is text — use C-x r i"))))))

(defn capture
  "Handle the character requested by a register prompt."
  [{tag :capture-next :as s} key]
  (let [s (dissoc s :capture-next)]
    (if-not (char? key)
      (cmd/msg s (str "Not a register: " key))
      (case tag
        :reg-point
        (-> s (assoc-in [:registers key] {:kind :point :buf (:buf s) :pos (:point (cmd/cur s))})
              (cmd/msg (str "Point saved in register " key)))

        (:reg-jump :reg-insert) (use-register s key tag)

        :reg-copy
        (if-let [[lo hi] (cmd/region-bounds s)]
          (-> s (assoc-in [:registers key] {:kind :text :text (gap/substr (:text (cmd/cur s)) lo hi)})
                (cmd/msg (str "Region copied to register " key)))
          (cmd/msg s "No region"))

        :reg-window
        (-> s (assoc-in [:registers key]
                        {:kind :window :windows (:windows s) :cur-window (:cur-window s)})
              (cmd/msg (str "Window configuration saved in register " key)))))))
