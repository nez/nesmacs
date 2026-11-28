(ns nesmacs.language
  "Editor commands for LSP and symbol completion."
  (:require [clojure.string :as str]
            [nesmacs.api :as api]
            [nesmacs.buf :as buf]
            [nesmacs.cmd :as cmd]
            [nesmacs.complete :as complete]
            [nesmacs.files :as files]
            [nesmacs.gap :as gap]
            [nesmacs.listbuf :as listbuf]
            [nesmacs.log :as log]
            [nesmacs.lsp :as lsp]
            [nesmacs.mini :as mini]
            [nesmacs.nav :as nav]
            [nesmacs.snippet :as snippet]))

(defn- symbol-prefix [text p]
  (let [before (gap/substr text (gap/line-start text p) p)]
    (re-find #"[$\w\-\*\+!\?\./#]*$" before)))

(defn- offset->pos
  "LSP position {:line :character} (both 0-based) for char offset `i`."
  [text i]
  (let [l (gap/line-of text i)]
    {:line l :character (- i (gap/nth-line-start text l))}))

(defn- completion-replacement
  "Compute [from to text caret] for accepting `item` with `prefix` at point."
  [s item prefix p]
  (let [edit (:textEdit item)
        range (lsp/range->offsets (:text (cmd/cur s)) (:range edit))
        [from to] (or range [(- p (count prefix)) p])
        text (buf/normalize-eol
               (str (or (when range (:newText edit)) (:insertText item) (:label item))))
        [text caret] (if (= 2 (:insertTextFormat item))
                       (snippet/expand text)
                       [text nil])]
    [from to text caret]))

(defn complete-symbol-at-point
  "C-c i: complete the symbol at point asynchronously from buffers and the
   project. The candidate is inserted replacing the prefix; LSP items honour
   textEdit and snippet insertTextFormat."
  [s]
  (let [b (cmd/cur s) p (:point b)
        source (:buf s)
        version (:version b)
        prefix (symbol-prefix (:text b) p)
        {:keys [line character]} (offset->pos (:text b) p)
        ch character]
    (mini/start s "Complete: "
      (fn [s chosen]
        (let [b (cmd/cur s)
              item (get (complete/document-items b [line ch]) chosen)]
          (if (and (= source (:buf s)) (= version (:version b)) (= p (:point b))
                   (= prefix (gap/substr (:text b) (- p (count prefix)) p)))
            (if item
              (let [[from to repl caret] (completion-replacement s item prefix p)]
                (-> s (cmd/edit from to repl)
                      (cmd/goto-char (+ from (long (or caret (count repl)))))))
              (cmd/edit s (- p (count prefix)) p chosen))
            (cmd/msg s "Buffer moved; completion skipped"))))
      {:sources (complete/sources-for s)})))

;; --- LSP commands ---

(defn- lsp-argv
  "Server argv for language `l`: per-language config wins, then the legacy
   global override, then the registry default."
  [s l]
  (or (get-in s [:lsp-commands (:id l)])
      (:lsp-command s)
      (:lsp l)))

(defn lsp-session
  "Resolve the current buffer's server and its configured command."
  [s]
  (when-let [sess (lsp/document-session (cmd/cur s))]
    (assoc sess :argv (lsp-argv s (:language sess)))))

(defn- char-col
  "0-based UTF-16 character index of point within its line — what LSP wants."
  [s]
  (let [b (cmd/cur s)]
    (:character (offset->pos (:text b) (:point b)))))

(defn- source-current? [s key source]
  (files/same-buffer-version? (cmd/buf s key) source))

(defn- lsp-command [s run]
  (let [{:keys [file large-file]} (cmd/cur s)]
    (cond
      (nil? file) (cmd/msg s "Buffer has no file")
      large-file (cmd/msg s "LSP is disabled in large-file buffers")
      :else
      (if-let [sess (lsp-session s)]
        (if (lsp/on-path? (:argv sess))
          (run sess)
          (cmd/msg s (str "Language server not on PATH: " (first (:argv sess)))))
        (cmd/msg s "No language server for this file")))))

(defn- ensure-document! [editor key source {:keys [root-id argv root language]} request]
  (lsp/ensure! root-id argv root (:id language)
               (fn [_]
                 (when (source-current? @editor key source)
                   (lsp/sync! root-id source)
                   (request)))))

(defn- lsp-request-at-point [editor s request]
  (lsp-command s
    (fn [sess]
      (api/after-command s
        (fn [_]
          (ensure-document! editor (:buf s) (cmd/cur s) sess #(request s sess)))))))

(defn- show-hover
  "Echo an LSP Hover result in the message bar."
  [editor result]
  (let [c   (:contents result)
        txt (cond (string? c) c
                  (map? c)   (:value c)
                  (seq c)    (str/join "\n" (map :value c)))
        txt (some-> txt (str/replace #"\s+" " "))]
    (when txt
      (swap! editor assoc :msg (str "LSP: " (subs txt 0 (min 200 (count txt))))))))

(defn- location->entry [loc]
  (let [uri (or (:uri loc) (:targetUri loc))
        start (:start (or (:targetSelectionRange loc) (:range loc) (:targetRange loc)))]
    (when-let [file (and uri start (lsp/uri->path uri))]
      {:file file :line (inc (max 0 (:line start 0))) :character (:character start)})))

(defn- jump-to-definition [editor res]
  (when-let [entry (location->entry (if (vector? res) (first res) res))]
    (swap! editor nav/jump nil entry)))

(defn- lsp-position-request [editor s request on-result]
  (lsp-request-at-point editor s
    (fn [state {:keys [root-id]}]
      (request root-id (:file (cmd/cur state))
               (cmd/line-idx state) (char-col state) (partial on-result editor)))))

(defn lsp-hover [editor s]
  (lsp-position-request editor s lsp/hover show-hover))

(defn lsp-definition [editor s]
  (lsp-position-request editor s lsp/definition jump-to-definition))

(defn- show-references [editor res]
  (let [entries (vec (keep location->entry res))]
    (swap! editor
           (fn [s]
             (if (empty? entries)
               (cmd/msg s "No references")
               (listbuf/open s "*References*"
                             {:name "*References*"
                              :mode :nav-mode
                              :header (str (count entries) " references")
                              :entries entries
                              :format-entry (fn [{:keys [file line]}]
                                              (format "%s:%d" file line))
                              :extras {:nav-root nil}}))))))

(defn lsp-references [editor s]
  (lsp-position-request editor s lsp/references show-references))

;; --- Workspace edits (rename, code actions, formatting) ---

(defn- workspace-source [state path]
  (if-let [key (files/file-buffer-key state path)]
    (let [b (cmd/buf state key)]
      {:key key :buffer b :text (:text b)})
    (try
      (let [{:keys [raw buffer]} (files/read-document (.getName (java.io.File. ^String path)) path)]
        {:key path :expected raw :buffer buffer :text (:text buffer)})
      (catch Exception e
        (log/log "workspace edit read failed:" path (.getMessage e))
        nil))))

(defn- workspace-source-current? [state path {:keys [key buffer expected]}]
  (if expected
    (nil? (files/file-buffer-key state path))
    (files/same-buffer-version? (cmd/buf state key) buffer)))

(defn- apply-edit-group [state {:keys [path edits source]}]
  (if (workspace-source-current? state path source)
    (let [state (if (:expected source)
                  (-> (files/open-buf state path (:buffer source))
                      (files/bump-recentf path))
                  state)]
      (update-in state [:bufs (:key source)]
                 (fn [b] (reduce (fn [b {:keys [from to text]}]
                                   (buf/edit b from to text))
                                 b edits))))
    state))

(defn apply-workspace-edit!
  "Apply a WorkspaceEdit to open buffers, writing previously unopened files
   only if their source on disk still matches the bytes used for the edit."
  [editor edit]
  (let [initial @editor
        source (memoize #(workspace-source initial %))
        groups (mapv #(assoc % :source (source (:path %)))
                     (lsp/workspace-edits edit #(:text (source %))))]
    (if (empty? groups)
      (swap! editor cmd/msg "No edits to apply")
      (let [[before after] (swap-vals! editor #(reduce apply-edit-group % groups))
            applied (filter #(workspace-source-current? before (:path %) (:source %)) groups)
            written (count (filter (fn [{:keys [path source]}]
                                     (when-let [expected (:expected source)]
                                       (try
                                         (files/write-buffer! (fn [f] (swap! editor f)) path
                                                        (cmd/buf after path) expected)
                                         true
                                         (catch Exception e
                                           (log/log "workspace edit write failed:" path (.getMessage e))
                                           false))))
                                   applied))]
        (swap! editor cmd/msg (str "Applied edits in " (count applied) " file(s)"
                               (when (pos? written) (str ", " written " written"))
                               (when (< (count applied) (count groups))
                                 " (some buffers changed)")))))))

(defn- apply-code-action!
  [editor sess action]
  (let [cmd (:command action)]
    (when-let [edit (:edit action)] (apply-workspace-edit! editor edit))
    (when cmd
      (lsp/execute-command (:root-id sess) (:command cmd) (:arguments cmd)
                           (fn [r] (when (map? r) (apply-workspace-edit! editor r)))))))

(defn- diags-in-range
  "LSP diagnostics from the buffer's LSP props overlapping [lo hi), with
   positions as {:line :character} maps built by `pos`."
  [buf lo hi pos]
  (->> (:props buf)
       (filter (fn [p] (and (= :lsp (:owner p))
                            (<= (long (:from p)) (long hi))
                            (>= (long (:to p)) (long lo)))))
       (mapv (fn [p]
               {:range {:start (pos (:from p)) :end (pos (:to p))}
                :severity (:severity p)
                :message (str (:message p))
                :source "lsp"}))))

(defn lsp-code-action [editor s]
  (lsp-request-at-point editor s
    (fn [state sess]
      (let [t (:text (cmd/cur state))
            ln (cmd/line-idx state)
            [lo hi] (or (cmd/region-bounds state)
                        [(gap/nth-line-start t ln) (gap/nth-line-end t ln)])
            pos (fn [i] (offset->pos t i))
            range {:start (pos lo) :end (pos hi)}
            diags (diags-in-range (cmd/cur state) lo hi pos)]
        (lsp/code-action (:root-id sess) (:file (cmd/cur state)) range diags
          (fn [res]
            (let [actions (vec (filter :title res))]
              (cond
                (empty? actions) (swap! editor cmd/msg "No code actions")
                (= 1 (count actions)) (apply-code-action! editor sess (first actions))
                :else
                (swap! editor
                       (fn [s]
                         (mini/start
                           s "Code action: "
                           (fn [st input]
                             (if-let [a (first (filter #(= input (:title %)) actions))]
                               (api/after-command st (fn [_] (apply-code-action! editor sess a)))
                               st))
                           (fn [input _]
                             (mini/complete-prefix input (map :title actions))))))))))
        state))))

(defn- show-signature [editor {:keys [signatures activeSignature]}]
  (let [sig (get signatures (max 0 (or activeSignature 0)))]
    (when-let [label (or (:label sig) (:label (first signatures)))]
      (swap! editor cmd/msg (str "LSP: " label)))))

(defn lsp-signature-help [editor s]
  (lsp-position-request editor s lsp/signature-help show-signature))

(defn- symbol-entries
  "Flatten DocumentSymbol trees (or a flat SymbolInformation vector)."
  [res]
  (letfn [(walk [items]
            (mapcat (fn [d]
                      (let [name (str (:name d))
                            line (or (get-in d [:selectionRange :start :line])
                                     (get-in d [:location :range :start :line])
                                     (get-in d [:range :start :line]) 0)
                            entry (cond-> {:name name :line (inc line)}
                                    (get-in d [:location :uri])
                                    (assoc :file (lsp/uri->path (get-in d [:location :uri]))))]
                        (cons entry (walk (:children d)))))
                    items))]
    (vec (walk res))))

(defn lsp-document-symbols [editor s]
  (lsp-request-at-point editor s
    (fn [state sess]
      (lsp/document-symbol (:root-id sess) (:file (cmd/cur state))
                           (fn [res]
                             (let [entries (symbol-entries res)]
                               (swap! editor
                                      (fn [s]
                                        (if (empty? entries)
                                          (cmd/msg s "No symbols")
                                          (listbuf/open
                                            s "*Imenu*"
                                            {:name "*Imenu*"
                                             :mode :nav-mode
                                             :header (str (count entries) " symbols")
                                             :entries (mapv #(assoc % :buf (:buf state)) entries)
                                             :format-entry (fn [{:keys [line name]}]
                                                             (format "%5d  %s" line name))
                                             :extras {:nav-root nil}})))))))
      state)))

(defn- request-format [editor s region]
  (lsp-request-at-point editor s
    (fn [state {:keys [root-id]}]
      (let [{:keys [file text language]} (cmd/cur state)
            options {:tabSize (or (:indent language) 2) :insertSpaces true}
            on-edits (fn [edits]
                       (if (source-current? @editor (:buf state) (cmd/cur state))
                         (if (seq edits)
                           (apply-workspace-edit! editor {:changes {(lsp/path->uri file) edits}})
                           (swap! editor cmd/msg "Already formatted"))
                         (swap! editor cmd/msg "Buffer changed; formatting skipped")))]
        (if-let [[lo hi] region]
          (let [range {:start (offset->pos text lo) :end (offset->pos text hi)}]
            (lsp/range-formatting root-id file range options on-edits))
          (lsp/formatting root-id file options on-edits)))
      state)))

(defn lsp-format [editor s]
  (request-format editor s nil))

(defn lsp-format-region [editor s]
  (if-let [region (cmd/region-bounds s)]
    (request-format editor s region)
    (cmd/msg s "No region")))

;; --- Rename ---

(defn lsp-rename [editor s]
  (lsp-command s
    (fn [sess]
      (let [source (cmd/cur s)
            key (:buf s)
            path (:file source)
            line (cmd/line-idx s)
            col (char-col s)]
        (mini/start s "Rename to: "
          (fn [st new-name]
            (cond
              (str/blank? new-name) st
              (not (source-current? st key source)) (cmd/msg st "Buffer changed; rename skipped")
              :else
              (api/after-command st
                (fn [_]
                  (ensure-document! editor key source sess
                    #(lsp/rename (:root-id sess) path line col new-name
                       (fn [edit]
                         (when (source-current? @editor key source)
                           (if edit
                             (apply-workspace-edit! editor edit)
                             (swap! editor cmd/msg "Rename failed")))))))))))))))

;; Completion is cache + push, never blocking: a cache miss starts one async
;; request and returns the cached hits (usually none); when the reply lands we
;; stash it and nudge the minibuffer to re-run its sources, which now hit the
;; cache. So a slow server only delays the *list*, never a keystroke.
(defn lsp-completion-source [editor prefix {:keys [state]}]
  (if-let [{:keys [root-id root language argv]} (lsp-session state)]
    (cond
      (not (lsp/server-exists? root-id))
      (do (when (lsp/on-path? argv)
            (lsp/ensure! root-id argv root (:id language) nil))
          [])

      (lsp/ready? root-id)
      (let [line (cmd/line-idx state)
            ch (char-col state)
            file (:file (cmd/cur state))]
        (complete/document-candidates! editor state [line ch] prefix
          #(lsp/completion root-id file line ch %)))

      :else [])
    []))
