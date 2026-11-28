(ns nesmacs.job
  "Process-backed buffers. Output streams in through `pump!`, which the editor
   loop calls between keystrokes, so the process never touches editor state on
   its own thread and the UI stays responsive."
  (:require [nesmacs.buf :as buf]
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
  (let [n (count (:text b))]
    (cond->
      (cond
        (:job-truncated b) b
        (>= n max-output) (assoc b :job-truncated true :modified false)
        (> (+ n (count text)) max-output)
        (-> (buf/edit b n n (subs text 0 (- max-output n)))
            (buf/edit max-output max-output "\n[output truncated]\n")
            (assoc :job-truncated true :modified false))
        :else (assoc (buf/edit b n n text) :modified false))
      ;; Appending normally moves a point at EOF. Search starts at the first hit.
      (= false (:job-follow-output? b)) (assoc :point (:point b)))))

(defn start
  "Start `argv` with its output going to buffer `buf-name`, creating the buffer
   (and showing it) if needed. `:mode` defaults to :job-mode. `argv` is the
   exact argument vector. Set :follow-output? false to keep point on the results."
  [s {:keys [argv dir buf-name mode follow-output?]
      :or {follow-output? true}}]
  (let [b (-> (buf/make buf-name "" nil)
              (assoc :mode (or mode :job-mode) :read-only true :journal? false
                     :job-command (vec argv) :job-dir dir :job-status :running
                     :job-follow-output? follow-output?))]
    (try
      (let [h (proc/start {:argv argv :dir dir :kind :job})]
        (-> s
            (update :bufs assoc buf-name (assoc b :job-proc (:id h)))
            (cmd/set-cur-buffer buf-name)))
      (catch Exception e
        ;; e.g. rg not installed, or a vanished cwd: show it in the buffer
        ;; instead of letting the exception escape the command loop.
        (-> s
            (update :bufs assoc buf-name
                    (-> b (assoc :job-status :failed)
                          (buf/edit 0 0 (str "Error: " (.getMessage e) "\n"))
                          (assoc :modified false)))
            (cmd/set-cur-buffer buf-name))))))

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
  "Prepare results for matching job buffers outside the editor swap. Publish
   only while the same output is still pending. Each run owns a fresh Text,
   and changes to focus or fontification leave that Text in place."
  [editor pending? prepare publish]
  (doseq [[name b] (:bufs @editor)
          :when (pending? b)]
    (let [result (prepare b)]
      (swap! editor
             (fn [s]
               (let [current (cmd/buf s name)]
                 (if (and (pending? current)
                          (identical? (:text b) (:text current)))
                   (publish s name current result)
                   s)))))))

(defn kill
  "C-c C-k: kill the process behind the current job buffer."
  [s]
  (let [b (cmd/cur s)]
    (if-let [h (some-> (:job-proc b) proc/by-id)]
      (do (proc/kill! h)
          (-> s (update-in [:bufs (:name b)] assoc :job-status :killed :job-proc nil)
                (cmd/msg "Killed process")))
      (cmd/msg s "No running process"))))

(defn rerun
  "`g`: run the buffer's command again into the same buffer."
  [s]
  (let [b (cmd/cur s)]
    (if-let [argv (:job-command b)]
      (do (when-let [h (some-> (:job-proc b) proc/by-id)] (proc/kill! h))
          (start s {:argv argv :dir (:job-dir b) :buf-name (:name b)
                    :mode (:mode b) :follow-output? (:job-follow-output? b)}))
      (cmd/msg s "Not a job buffer"))))
