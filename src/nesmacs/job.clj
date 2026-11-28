(ns nesmacs.job
  "Process-backed buffers. Output streams in through `pump!`, which the editor
   loop calls between keystrokes, so the process never touches editor state on
   its own thread and the UI stays responsive."
  (:require [nesmacs.api :as api]
            [nesmacs.buf :as buf]
            [nesmacs.cmd :as cmd]
            [nesmacs.proc :as proc]))

(def ^:private max-output
  "Cap per job buffer. shortcut: fixed 2MB, enough for a `find /`; make it a
   buffer field if a real workload needs more."
  2000000)

(def ^:private pump-limit
  "Characters read per job per tick, so a firehose can't starve the keyboard."
  200000)

(defn- append-text [b ^String text]
  (if (or (:job-truncated b) (empty? text))
    b
    (let [space (max 0 (- max-output (count (:text b))))
          truncated? (> (count text) space)
          chunk (if truncated? (str (subs text 0 space) "\n[output truncated]\n") text)]
      (cond-> (buf/append-output b chunk)
        truncated? (assoc :job-truncated true)
        (= false (:job-follow-output? b)) (assoc :point (:point b))))))

(defn- kill-after-command [s id]
  (cond-> s
    id (api/after-command (fn [_] (proc/kill! (proc/by-id id))))))

(defn- launch! [publish! {:keys [argv dir env buf-name]} token]
  (let [path [:bufs buf-name]
        current? #(identical? token (get-in % (conj path :job-token)))]
    (when (current? (publish! identity))
      (let [{:keys [handle error]}
            (try {:handle (proc/start {:argv argv :dir dir :env env :kind :job})}
                 (catch Exception e {:error (str "Error: " (.getMessage e) "\n")}))
            state (publish! (fn [s]
                              (if (current? s)
                                (update-in s path
                                           (fn [b]
                                             (if handle
                                               (assoc b :job-proc (:id handle))
                                               (-> b (append-text error)
                                                   (assoc :job-status :failed)))))
                                s)))]
        (when (and handle (not (current? state)))
          (proc/kill! handle))))))

(defn start
  "Set up a job buffer and launch its command after commit. The launch spec
   includes :buffer-fields recreated on each rerun, in addition to :mode and
   :follow-output? (defaults to true)."
  [s {:keys [argv dir buf-name mode follow-output? buffer-fields on-finish]
      :or {follow-output? true}
      :as spec}]
  (let [spec (assoc spec :argv (vec argv))
        token (Object.)
        b (-> (buf/make buf-name "" nil)
              (merge buffer-fields)
              (assoc :mode (or mode :job-mode) :read-only true :journal? false
                     :job-spec spec :job-dir dir :job-status :running
                     :job-token token :job-follow-output? follow-output?
                     :job-finish on-finish))]
    (-> s
        (kill-after-command (:job-proc (cmd/buf s buf-name)))
        (assoc-in [:bufs buf-name] b)
        (cmd/set-cur-buffer buf-name)
        (api/after-command (fn [publish!] (launch! publish! spec token))))))

(defn drain!
  "Take every available chunk and the finished state out of the processes,
   without touching editor state. A swap! that retries must not re-run this:
   the chunks were already removed from the queue and a discarded state would
   lose them."
  [bufs]
  (proc/kill-orphans! :job (set (keep :job-proc (vals bufs))))
  (reduce-kv
    (fn [acc nm b]
      (if-let [h (some-> (:job-proc b) proc/by-id)]
        (let [chunk (proc/drain! h pump-limit)
              done? (proc/finished? h)
              code  (when done? (long (or (proc/exit-code h) 0)))]
          (when done? (proc/forget! h))
          (cond-> acc
            (seq chunk) (assoc-in [nm :text] chunk)
            done? (assoc-in [nm :exit] code)
            true (assoc-in [nm :proc-id] (:job-proc b))))
        acc))
    {} bufs))

(defn apply-drain
  "Pure counterpart of `drain!`. `:proc-id` guards against a rerun that replaced
   the buffer's process between the drain and this swap."
  [s drained]
  (reduce-kv
    (fn [s nm {:keys [text exit proc-id]}]
      (let [b (get-in s [:bufs nm])]
        (if-not (and b (= proc-id (:job-proc b)))
          s
          (let [s (cond-> s (seq text) (update-in [:bufs nm] append-text text))]
            (if (some? exit)
              (let [s (update-in s [:bufs nm]
                                 assoc :job-status :done :job-proc nil :job-exit exit)]
                (if (zero? (long exit))
                  s
                  (update-in s [:bufs nm] append-text (str "\n[exit " exit "]\n"))))
              s)))))
    s drained))

(defn pump!
  "Drain live job output into their buffers and reap finished processes.
   Call from the editor loop only: it is the single writer for job text."
  [s]
  (apply-drain s (drain! (:bufs s))))

(defn consume-results!
  "Prepare the result outside swap! and apply it only to the run that produced it."
  [editor]
  (doseq [[name b] (:bufs @editor)
          :let [prepare (:job-finish b)]
          :when (and prepare (#{:done :failed} (:job-status b)))]
    (let [publish (try (prepare b)
                       (catch Exception e
                         (fn [s _] (cmd/msg-error s "Job result failed" e))))]
      (swap! editor
             (fn [s]
               (let [current (cmd/buf s name)]
                 (if (and (identical? prepare (:job-finish current))
                          (identical? (:job-token b) (:job-token current))
                          (identical? (:text b) (:text current)))
                   (cond-> (update-in s [:bufs name] dissoc :job-finish)
                     publish (publish name))
                   s)))))))

(defn kill
  "C-c C-k: kill the process behind the current job buffer."
  [s]
  (let [b (cmd/cur s)]
    (if (= :running (:job-status b))
      (-> s
          (kill-after-command (:job-proc b))
          (cmd/update-cur #(-> % (assoc :job-status :killed)
                                 (dissoc :job-proc :job-token)))
          (cmd/msg "Killed process"))
      (cmd/msg s "No running process"))))

(defn rerun
  "`g`: run the buffer's command again into the same buffer."
  [s]
  (let [b (cmd/cur s)]
    (if-let [spec (or (:job-spec b)
                      ;; The dev watcher can reload code with old buffers still open.
                      (when-let [argv (:job-command b)]
                        {:argv argv :dir (:job-dir b) :buf-name (:name b)
                         :mode (:mode b) :follow-output? (:job-follow-output? b)
                         :on-finish (:job-finish b)}))]
      (start s spec)
      (cmd/msg s "Not a job buffer"))))
