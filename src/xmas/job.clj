(ns xmas.job
  "Process-backed buffers. Output streams in through `pump!`, which the editor
   loop calls between keystrokes, so the process never touches editor state on
   its own thread and the UI stays responsive."
  (:require [xmas.buf :as buf]
            [xmas.cmd :as cmd]
            [xmas.proc :as proc]))

(def ^:private max-output
  "Cap per job buffer. shortcut: fixed 2MB, enough for a `find /`; make it a
   buffer field if a real workload needs more."
  2000000)

(def ^:private pump-limit
  "Characters read per job per tick, so a firehose can't starve the keyboard."
  200000)

(defn- append-text [b ^String text]
  (let [n (count (:text b))]
    (cond
      (:job-truncated b) b
      (>= n max-output)  (-> b (assoc :job-truncated true :modified false))
      (> (+ n (count text)) max-output)
      (-> (buf/edit b n n (subs text 0 (- max-output n)))
          (buf/edit max-output max-output "\n[output truncated]\n")
          (assoc :job-truncated true :modified false))
      :else
      ;; buf/edit shifts point by the insertion, so a point at the end follows
      ;; output while a point elsewhere stays put.
      (-> (buf/edit b n n text) (assoc :modified false)))))

(defn start
  "Start `argv` with its output going to buffer `buf-name`, creating the buffer
   (and showing it) if needed. `:mode` defaults to :job-mode. `argv` is the
   exact argument vector."
  [s {:keys [argv dir buf-name mode]}]
  (let [b (-> (buf/make buf-name "" nil)
              (assoc :mode (or mode :job-mode) :read-only true
                     :job-command (vec argv) :job-dir dir :job-status :running))]
    (try
      (let [h (proc/start {:argv argv :dir dir})]
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

(defn pump!
  "Drain live job output into their buffers and reap finished processes.
   Call from the editor loop only: it is the single writer for job text."
  [s]
  (proc/kill-orphans! (set (keep :job-proc (vals (:bufs s)))))
  (reduce-kv
    (fn [s nm b]
      (if-let [h (some-> (:job-proc b) proc/by-id)]
        (let [chunk (proc/drain! h pump-limit)
              s     (cond-> s (seq chunk) (update-in [:bufs nm] append-text chunk))]
          (if (proc/finished? h)
            (let [code (long (or (proc/exit-code h) 0))
                  b    (-> (get-in s [:bufs nm])
                           (assoc :job-status :done :job-proc nil))
                  s    (assoc-in s [:bufs nm] b)]
              (proc/forget! h)
              (if (zero? code)
                s
                (update-in s [:bufs nm] append-text (str "\n[exit " code "]\n"))))
            s))
        s))
    s (:bufs s)))

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
          (start s {:argv argv :dir (:job-dir b) :buf-name (:name b)}))
      (cmd/msg s "Not a job buffer"))))