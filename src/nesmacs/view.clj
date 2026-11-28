(ns nesmacs.view
  (:require [clojure.string :as str]
            [nesmacs.face :as face]
            [nesmacs.fold :as fold]
            [nesmacs.gap :as gap]
            [nesmacs.props :as props]
            [nesmacs.term :as t]
            [nesmacs.text :as text]
            [nesmacs.treesit :as treesit]
            [nesmacs.window :as win]))

(def default-faces
  {:default   {:fg nil :bg nil :bold false}
   :modeline  {:fg 235 :bg 108 :bold true}
   :inactive  {:fg 241 :bg 236 :bold false}
   :prompt    {:fg 109 :bg nil :bold true}
   :region    {:fg nil :bg 24  :bold false}
   :divider   {:fg 240 :bg nil :bold false}
   :comment   {:fg 102 :bg nil :bold false}
   :string    {:fg 107 :bg nil :bold false}
   :keyword   {:fg 180 :bg nil :bold false}
   :builtin   {:fg 67  :bg nil :bold true}
   :function  {:fg 81  :bg nil :bold false}
   :type      {:fg 215 :bg nil :bold false}
   :variable  {:fg 189 :bg nil :bold false}
   :number    {:fg 176 :bg nil :bold false}
   :constant  {:fg 173 :bg nil :bold false}
   :operator  {:fg 146 :bg nil :bold false}
   :fold      {:fg 244 :bg nil :bold false}
   :match     {:fg 232 :bg 180 :bold false}
   :diag-error   {:fg 231 :bg 124 :bold true}
   :diag-warning {:fg 232 :bg 136 :bold false}
   :diag-info    {:fg 232 :bg 109 :bold false}
   :diff-add     {:fg 120 :bg nil :bold false}
   :diff-del     {:fg 167 :bg nil :bold false}
   :diff-hunk    {:fg 67  :bg nil :bold true}
   :diff-meta    {:fg 244 :bg nil :bold false}
   :breakpoint   {:fg 196 :bg nil :bold true}
   :debug-stop   {:fg 226 :bg nil :bold true}})

(def light-faces
  (assoc default-faces
         :default   {:fg 236 :bg nil :bold false}
         :modeline  {:fg 255 :bg 31 :bold true}
         :inactive  {:fg 245 :bg 253 :bold false}
         :prompt    {:fg 25 :bg nil :bold true}
         :region    {:fg nil :bg 195 :bold false}
         :divider   {:fg 250 :bg nil :bold false}
         :comment   {:fg 245 :bg nil :bold false}
         :string    {:fg 28 :bg nil :bold false}
         :keyword   {:fg 90 :bg nil :bold false}
         :builtin   {:fg 25 :bg nil :bold true}
         :function  {:fg 25 :bg nil :bold false}
         :type      {:fg 130 :bg nil :bold false}
         :variable  {:fg 238 :bg nil :bold false}
         :number    {:fg 166 :bg nil :bold false}
         :constant  {:fg 130 :bg nil :bold false}
         :operator  {:fg 95 :bg nil :bold false}
         :match     {:fg 232 :bg 222 :bold false}
         :diag-warning {:fg 232 :bg 222 :bold false}
         :diag-info    {:fg 232 :bg 195 :bold false}
         :diff-add     {:fg 28 :bg nil :bold false}
         :diff-del     {:fg 124 :bg nil :bold false}
         :diff-hunk    {:fg 25 :bg nil :bold true}
         :diff-meta    {:fg 245 :bg nil :bold false}
         :breakpoint   {:fg 160 :bg nil :bold true}
         :debug-stop   {:fg 94 :bg nil :bold true}))

(def themes
  "Named palettes for `use-theme!`. :default is the shipped dark palette."
  {:default default-faces :light light-faces})

(def faces
  "The active face map, read by every painter. Changed only through
   `use-theme!` and `set-face-attr!`, which also bump `theme-version` so the
   next frame repaints."
  default-faces)

(defonce ^:private theme-version (atom 0))
(defonce ^:private applied-theme (atom nil))

(defn theme-names [] (vec (keys themes)))

(defn use-theme!
  "Switch to a named palette; unknown names are ignored. Called every tick from
   the editor loop, so it no-ops unless the theme actually changed."
  [theme]
  (when-let [f (get themes theme)]
    ;; The identity check re-applies the palette after a `require :reload`
    ;; redefines `faces`; `applied-theme` alone would leave it at the default.
    (when (or (not= theme @applied-theme) (not (identical? f faces)))
      (reset! applied-theme theme)
      (alter-var-root #'faces (constantly f))
      (swap! theme-version inc))
    theme))

(defn set-face-attr!
  "Merge `attrs` ({:fg :bg :bold}) into one face. For ad-hoc tweaks in init.clj;
   :default keeps its nil fg/bg."
  [face attrs]
  (alter-var-root #'faces update face
                  #(merge (or % (get faces :default)) attrs))
  (swap! theme-version inc)
  nil)

(defn display-row [folds line]
  (fold/visible-before folds (fold/visible-start folds line)))

(defn text-column [gutter-width hscroll]
  (+ gutter-width (if (pos? hscroll) 1 0)))

(defn visible-line-start [content start end hscroll]
  (if (pos? hscroll) (text/pos-at-col content start end hscroll) start))

(defn- ensure-visible [content point-line scroll rows folds]
  ;; A pane with body-rows <= 0 (e.g. a 1-row split that gets 0 body rows
  ;; after reserving the modeline) has nowhere to show the point, so
  ;; scrolling it produces out-of-range line indices — the old "anchor on
  ;; last row" branch used to compute nth-line-start of (inc last-line)
  ;; and crash with ArrayIndexOutOfBoundsException.
  (if (<= rows 0)
    scroll
    (let [pd (display-row folds point-line)
          sd (display-row folds (gap/line-of content scroll))]
      (cond
        (< pd sd)
        (gap/nth-line-start content (fold/line-at-display folds pd))

        (>= (- pd sd) rows)
        (gap/nth-line-start content
                            (fold/line-at-display folds (- pd (dec rows))))

        :else scroll))))

(defn- ensure-hvisible ^long [^long hscroll ^long cc ^long cols]
  ;; When hscroll > 0, column 0 is reserved for the `$` marker, so only
  ;; (cols - 1) columns of text are visible. The cursor is placed at
  ;; col-in-win = cc - hscroll + 1, so it fits iff cc < hscroll + cols - 1.
  ;; On overflow, pick hscroll so cc lands on the last visible column.
  (cond
    (< cc hscroll) cc
    (>= cc (+ hscroll cols (if (pos? hscroll) -1 0))) (+ cc 2 (- cols))
    :else hscroll))

;; --- Low-level writing within a rect ---

(defn- pad-spaces [n] (.repeat " " (max 0 (long n))))

(defn- set-face! [face]
  (t/sg (if (map? face) face (get faces face (:default faces)))))

(defn- safe-text [^String s]
  (str/replace s #"[\x00-\x1f\x7f-\x9f]" "�"))

(defn- write-spans!
  "Write row at (screen-row, screen-col) with face spans. Content is clipped
   to `width` columns (at most), and remaining columns are padded with spaces.
  `hscroll` is the horizontal offset into glyphs (0 for modeline etc.). When
  `fold?` is true, an ellipsis marks a collapsed range."
  [screen-row screen-col width glyphs spans hscroll & [fold?]]
  (when (pos? width)
    (t/move screen-row screen-col)
    (let [n (count glyphs)
          scrolled (pos? hscroll)
          start (visible-line-start glyphs 0 n hscroll)
          avail (if scrolled (dec width) width)
          end   (text/pos-at-col glyphs start n avail)
          ;; visible display width used
          used-cols (+ (cond-> (text/display-width glyphs start end)
                         scrolled inc)
                       (if (and fold? (< (text/display-width glyphs start end) avail))
                         1 0))]
      (when scrolled (set-face! :default) (t/tw "$"))
      (doseq [[from to face] spans]
        (let [a (max (long from) start)
              b (min (long to) end)]
          (when (< a b)
            (set-face! face)
            (t/tw (safe-text (subs glyphs a b))))))
      (t/reset-sg)
      (when (and fold? (< (text/display-width glyphs start end) avail))
        (set-face! :fold) (t/tw "…"))
      (t/reset-sg)
      (when (< used-cols width)
        (t/tw (pad-spaces (- width used-cols)))))))

(defn- fill-row!
  "Emit `width` spaces with `face` at (screen-row, screen-col). Used to clear
   trailing window rows and to paint dividers."
  [screen-row screen-col width face]
  (when (pos? width)
    (t/move screen-row screen-col)
    (set-face! face)
    (t/tw (pad-spaces width))
    (t/reset-sg)))

(defn- paint-gutter!
  "Draw the left gutter for one body row in `gw` columns: the line's mark (if
   any) plus padding. Marks are buffer data (`:gutter`), not a second overlay."
  [screen-row screen-col gw mark]
  (when (pos? gw)
    (t/move screen-row screen-col)
    (let [txt (or (:text mark) "")
          shown (subs txt 0 (min (long gw) (count txt)))]
      (set-face! (if mark (:face mark) :divider))
      (t/tw shown)
      (t/reset-sg)
      (t/tw (pad-spaces (- (long gw) (count shown)))))))

(defn apply-face-range
  "Split `spans` at [from, to), painting that slice with `paint-face`.
   Returns spans unchanged for nil or empty ranges."
  [spans from to paint-face]
  (if (or (nil? from) (nil? to) (>= (long from) (long to)))
    spans
    (let [lo (long from) hi (long to)]
      (reduce
        (fn [acc [a b orig]]
          (let [a (long a) b (long b)]
            (if (or (<= b lo) (>= a hi))
              (conj acc [a b orig])
              (let [ma (max a lo) mb (min b hi)]
                (cond-> acc
                  (< a ma)   (conj [a ma orig])
                  (< ma mb)  (conj [ma mb paint-face])
                  (< mb b)   (conj [mb b orig]))))))
        []
        spans))))

(defn apply-props
  "Paint buffer-global props onto line-local spans. Props are clamped to the
   line and sorted by :priority ascending, so higher-priority ones paint last
   and win visually; ties keep insertion order."
  [spans props line-pos line-len]
  (reduce
    (fn [acc p]
      (let [from-local (max 0 (- (:from p) line-pos))
            to-local   (min line-len (- (:to p) line-pos))]
        (if (< from-local to-local)
          (apply-face-range acc from-local to-local (:face p))
          acc)))
    spans (sort-by #(or (:priority %) 0) props)))

;; --- Window rendering ---

(defn- line-str [^CharSequence content ^long pos ^long eol]
  (.toString (.subSequence content (int pos) (int eol))))

(defn- tokenize-at
  "Tokenize line `ln`, starting from lexer `state`."
  [mode content ^long ln state]
  (face/tokenize mode
                 (line-str content
                           (gap/nth-line-start content ln)
                           (gap/nth-line-end content ln))
                 state))

(defn- tokenize-line
  "Tokenize line `ln` using the state carried by the previous font-lock entry.
   Returns [spans state-after-line]."
  [mode content font-lock ^long ln]
  (let [state (if (zero? ln) :normal (second (nth font-lock (dec ln))))]
    (tokenize-at mode content ln state)))

(defn- extend-font-lock
  "Ensure `font-lock` has entries up through line (target-line - 1). Entry i
   is [spans state-after-line-i]; spans is nil for lines tokenized only to
   carry the lexer state. Returns [font-lock state-before-target-line].

   Spans are cached only for the current viewport: prune-font-lock drops them
   on scroll, while the cheap lexer states stay so scrolling never restarts
   from line 0."
  [mode content font-lock ^long target-line]
  (if (zero? target-line)
    [font-lock :normal]
    (let [n (count font-lock)]
      (if (>= n target-line)
        [font-lock (second (nth font-lock (dec target-line)))]
        (loop [ln n font-lock font-lock]
          (if (>= ln target-line)
            [font-lock (second (nth font-lock (dec target-line)))]
            (let [[_ nxt] (tokenize-line mode content font-lock ln)]
              (recur (inc ln) (conj font-lock [nil nxt])))))))))

(defn- line-font-spans
  "Fontification spans for visible line `ln`: reuse the cached entry or
   tokenize the line and replace the entry. Returns [spans next-state cache']."
  [mode content font-lock ln state]
  (let [entry (when (< ln (count font-lock)) (nth font-lock ln))]
    (if (and entry (some? (first entry)))
      [(first entry) (second entry) font-lock]
      (let [[spans nxt] (tokenize-at mode content ln state)
            entry' [spans nxt]]
        [spans nxt (if entry (assoc font-lock ln entry') (conj font-lock entry'))]))))

(defn- prune-font-lock
  "Drop cached spans for lines outside the new viewport [start end), keeping
   their lexer states. Spans are the heavy part; states must stay so scrolling
   can resume without re-tokenizing from line 0.

   shortcut: one cache window per buffer, so two windows on the same buffer
   at different scroll positions evict each other's spans and re-tokenize
   their visible lines. Correct, just not cached; upgrade to a per-window
   window if split-screen editing of one buffer becomes common."
  [font-lock window start end]
  (if-not window
    font-lock
    (let [[old-start old-end] window
          n (count font-lock)
          outside (concat (range old-start (min old-end start))
                          (range (max old-start end) old-end))]
      (reduce (fn [fl ln]
                (if (and (< ln n) (some? (first (nth fl ln))))
                  (assoc fl ln [nil (second (nth fl ln))])
                  fl))
              font-lock outside))))

(defn- render-window-body
  "Render visible lines and return the possibly-extended :font-lock cache and
   its viewport window. If large-file? is true, skip fontification and buffer
   props for performance; the region still paints because it is a render-time
   prop. `folds` are the collapsed ranges to skip; `collapsed` marks the lines
   that show an ellipsis. `hl-spans` are whole-buffer Tree-sitter spans (nil
   falls back to the line tokenizer). `gutter` is {:width n :marks {line mark}}
   drawn to the left of the text, taking its columns from `cols`."
  [rect content mode props region scroll hscroll font-lock window large-file?
   folds collapsed hl-spans gutter]
  (let [{:keys [row col rows cols]} rect
        gw (long (:width gutter 0))
        tcol (+ (long col) gw)
        tcols (- (long cols) gw)
        marks (:marks gutter)
        body-rows (dec rows)
        start-line  (fold/visible-start folds (gap/line-of content scroll))
        total-lines (gap/line-count content)
        use-ts? (and (some? hl-spans) (not large-file?))
        [font-lock0 init-state] (if (or large-file? use-ts?)
                                  [(or font-lock []) :normal]
                                  (extend-font-lock mode content (or font-lock []) start-line))
        result
        (loop [ln start-line r 0 state init-state font-lock font-lock0 last-line start-line]
          (if (and (< r body-rows) (< ln total-lines))
            (let [pos  (gap/nth-line-start content ln)
                  eol  (gap/nth-line-end content ln)
                  line (line-str content pos eol)
                  len  (count line)
                  [toks nxt-state font-lock']
                  (if (or large-file? use-ts?)
                    [[[0 len :default]] :normal font-lock]
                    (line-font-spans mode content font-lock ln state))
                  hl (when use-ts? (treesit/line-spans hl-spans pos eol))
                  line-props (cond-> (if large-file? [] (props/in-range props pos eol))
                               region (conj region))
                  spans  (apply-props toks (concat hl line-props) pos len)]
              (if (fold/hidden? folds ln)
                (recur (inc ln) r nxt-state font-lock' last-line)
                (do
                  (paint-gutter! (+ row r) col gw (get marks ln))
                  (write-spans! (+ row r) tcol tcols line spans hscroll
                                (contains? collapsed ln))
                  (recur (inc ln) (inc r) nxt-state font-lock' (inc ln)))))
            {:r r :font-lock font-lock :last last-line}))]
    (loop [r (:r result)]
      (when (< r body-rows)
        (fill-row! (+ row r) col cols :default)
        (recur (inc r))))
    (if (or large-file? use-ts?)
      {:font-lock (:font-lock result) :font-lock-window nil}
      {:font-lock (prune-font-lock (:font-lock result) window
                                   start-line (:last result))
       :font-lock-window [start-line (:last result)]})))

(defn- render-window-modeline
  [rect buf-name modified mode minor-modes point-line cursor-col current?]
  (let [{:keys [row col rows cols]} rect
        minors (when (seq minor-modes)
                 (str " " (str/join " " (map clojure.core/name (sort minor-modes)))))
        ml (format " %s %s   L%d C%d  (%s%s)"
                   (if modified "**" "--") (or buf-name "")
                   (inc point-line) (inc cursor-col)
                   (clojure.core/name (or mode :fundamental))
                   (or minors ""))
        mw (text/display-width ml)
        padded (if (< mw cols)
                 (str ml (pad-spaces (- cols mw)))
                 (subs ml 0 (text/pos-at-col ml 0 (count ml) cols)))
        face (if current? :modeline :inactive)]
    (when (pos? rows)
      (write-spans! (+ row (dec rows)) col cols padded [[0 (count padded) face]] 0))))

(defn- render-one-window
  "Render one window into its rect. Returns
   {:window <updated window with scroll/hscroll/point-line/cursor-col>
    :buf-name <buffer key>
    :version <buffer version at render time>
    :font-lock <possibly-extended :font-lock cache>
    :font-lock-window <viewport whose spans are cached>}."
  [state window rect current?]
  (let [{:keys [rows cols]} rect
        body-rows (dec rows)
        buf-name (:buffer window)
        buf (get (:bufs state) buf-name)
        content (:text buf)
        point (:point buf)
        mark (:mark buf)
        mode (:mode buf)
        version (:version buf)
        large-file? (:large-file buf)
        point-line (gap/line-of content point)
        folds (fold/effective (fold/collapsed-ranges buf-name buf) point-line)
        scroll-in (:scroll window 0)
        hscroll-in (:hscroll window 0)
        scroll (if current? (ensure-visible content point-line scroll-in body-rows folds)
                   scroll-in)
        collapsed (into #{} (map first) folds)
        cursor-row (- (display-row folds point-line)
                      (display-row folds (gap/line-of content scroll)))
        cursor-col (text/display-width content (gap/nth-line-start content point-line) point)
        gutter (:gutter buf)
        gw (long (:width gutter 0))
        hscroll (if current? (ensure-hvisible hscroll-in cursor-col (- (long cols) gw)) hscroll-in)
        [r-lo r-hi] (when (and mark current?) (sort [(long mark) (long point)]))
        region (when (and r-lo (< r-lo r-hi))
                 (props/make r-lo r-hi :region {:owner :region :priority 100}))
        props (cond-> (:props buf)
                (and current? (:highlight-line? buf) (pos? point-line))
                (conj (props/make (gap/nth-line-start content point-line)
                                  (gap/nth-line-end content point-line)
                                  :region {:priority 10})))
        hl-spans (when (and (not large-file?) (:language buf))
                   (treesit/spans buf-name (:language buf) content version))
        {:keys [font-lock font-lock-window]}
        (render-window-body rect content mode props region scroll hscroll
                            (:font-lock buf) (:font-lock-window buf) large-file?
                            folds collapsed hl-spans gutter)]
    (render-window-modeline rect (or (:title buf) (:name buf)) (:modified buf) mode (:minor-modes buf)
                            point-line cursor-col current?)
    {:window (assoc window :scroll scroll :hscroll hscroll
                           :point-line point-line
                           :cursor-col (let [left (visible-line-start content
                                                    (gap/nth-line-start content point-line)
                                                    (gap/nth-line-end content point-line) hscroll)]
                                         (text/display-width content left point))
                           :cursor-row cursor-row :gutter-width gw :folds folds :version version)
     :buf-name buf-name
     :version version
     :font-lock font-lock
     :font-lock-window font-lock-window}))

;; --- Dividers ---

(defn- paint-divider! [{:keys [kind row col len]}]
  (case kind
    :horizontal (fill-row! row col len :divider)
    :vertical   (doseq [r (range len)]
                  (t/move (+ row r) col)
                  (t/sg (:divider faces))
                  (t/tw "│")
                  (t/reset-sg))))

;; --- Echo area ---

(defn- pad-to-cols! [used cols]
  (when (< used cols) (t/tw (pad-spaces (- cols used)))))

(defn- clip-to-cols
  "Clip `s` to at most `cols` display columns (CJK-aware)."
  [^String s ^long cols]
  (let [n (count s)
        end (text/pos-at-col s 0 n cols)]
    (if (< end n) (subs s 0 end) s)))

(defn- paint-candidates
  "Draw the completion candidate list above the echo area when a minibuffer
   is active and candidates are available. Selected line is highlighted with
   the :region face."
  [{:keys [rows cols mini] :as state}]
  (when (and mini (seq (:candidates mini)))
    (let [cs      (vec (:candidates mini))
          max-lines (min 8 (count cs) (max 0 (- rows 2)))
          sel     (get-in state [:mini :sel] -1)
          start   (max 0 (- (min (inc (max 0 sel)) (count cs)) max-lines))
          shown   (subvec cs start (+ start max-lines))]
      (doseq [i (range max-lines)
              :let [r (- rows 1 max-lines) c (clip-to-cols (str/trim (str (get shown i))) cols)]]
        (when c
          (t/move (+ r i) 0)
          (if (= (or sel -1) (+ start i))
            (t/sg (:region faces))
            (t/reset-sg))
          (t/tw c)
          (t/reset-sg)
          (pad-to-cols! (text/display-width c) cols))))))

(defn- echo-message [state]
  (let [mode (get-in state [:bufs (:buf state) :mode])]
    (or (:msg state) (get-in state [:modes mode :hint]) "")))

(defn- render-echo-area
  [{:keys [rows cols] :as state}]
  (let [mini (:mini state) isearch (:isearch state)
        mini-row (dec rows)]
    (t/move mini-row 0)
    (cond
      mini
      (let [{:keys [prompt candidates]} mini
            mb (get (:bufs state) (:buf state))
            input (str (:text mb))
            cursor (:point mb)
            pw (text/display-width prompt)
            avail (max 0 (- cols pw))
            input' (clip-to-cols input avail)
            input-w (text/display-width input')
            ;; After the input, if there's room, show completion candidates.
            hint (when (seq candidates)
                   (clip-to-cols (str "  {" (str/join " " candidates) "}")
                                 (max 0 (- cols pw input-w))))]
        (t/sg (:prompt faces)) (t/tw prompt)
        (t/reset-sg) (t/tw input')
        (when hint (t/tw hint))
        (pad-to-cols! (+ pw input-w (if hint (text/display-width hint) 0)) cols)
        (t/move mini-row (+ pw (min avail (text/display-width input 0 cursor)))))

      isearch
      (let [p (str (if (= (:direction isearch) :forward) "I-search: " "I-search backward: ")
                   (:pattern isearch))
            p' (clip-to-cols p cols)]
        (t/sg (:prompt faces)) (t/tw p') (t/reset-sg)
        (pad-to-cols! (text/display-width p') cols))

      :else
      (let [m (clip-to-cols (echo-message state) cols)]
        (t/reset-sg) (t/tw m)
        (pad-to-cols! (text/display-width m) cols)))))

;; --- Frame signature (dirty-flag input) ---

(defn- window-sig [state window current?]
  (let [buf (get (:bufs state) (:buffer window))]
    [(:buffer window) (:version buf) (:point buf) (:mark buf)
     (:mode buf) (:minor-modes buf) (:modified buf) (:name buf) (:title buf)
     (:props buf) (:scroll window) (:hscroll window) (:collapsed buf)
     (:gutter buf) (:highlight-line? buf) current?]))

(defn- echo-sig [state]
  (cond
    (:mini state)
    (let [mb (get (:bufs state) (:buf state))]
      [:mini (get-in state [:mini :prompt]) (str (:text mb)) (:point mb)
       (get-in state [:mini :candidates]) (get-in state [:mini :sel])])
    (:isearch state)
    [:isearch (:direction (:isearch state)) (:pattern (:isearch state))]
    :else [:msg (echo-message state)]))

(defn- frame-sig [state layouts cur-path]
  [(:rows state) (:cols state) layouts cur-path @theme-version
   (into {} (for [[path _] layouts]
              [path (window-sig state (get-in (:windows state) path)
                                (= path cur-path))]))
   (echo-sig state)])

;; --- Top-level render ---

(defn render
  "Render the whole frame. Returns its signature and the painted viewports. If the
   input signature equals (:render-sig state) the frame is skipped and only
   {:sig} is returned — the caller should write :sig back into state and
   leave scroll/hscroll untouched."
  [state]
  (let [tree (:windows state)
        {layouts :layout divs :dividers} (win/frame-geometry state)
        cur-path (:cur-window state)
        sig (frame-sig state layouts cur-path)]
    (if (= sig (:render-sig state))
      {:sig sig}
      (let [rendered (volatile! {})
            cache-ups (volatile! {})]
        (t/with-frame
          (t/hide-cur)
          (doseq [[path rect] layouts]
            (let [w (get-in tree path)
                  {:keys [window buf-name version font-lock font-lock-window]}
                  (render-one-window state w rect (= path cur-path))]
              (vswap! rendered assoc path window)
              (vswap! cache-ups
                      #(let [ex (get % buf-name)]
                         (if (and ex (>= (count (:font-lock ex)) (count font-lock)))
                           %
                           (assoc % buf-name
                                  {:version version
                                   :font-lock font-lock
                                   :font-lock-window font-lock-window}))))))
          (doseq [d divs] (paint-divider! d))
           (paint-candidates state)
          (render-echo-area state)
          (when-not (:mini state)
            (let [rect (get layouts cur-path)
                  win  (get @rendered cur-path)]
              (when (and rect win (> (:rows rect) 1) (pos? (:cols rect)))
                (let [body-rows (dec (:rows rect))
                      row-in-win (max 0 (min (long (:cursor-row win)) (dec body-rows)))
                      col-in-win (+ (:cursor-col win)
                                    (text-column (:gutter-width win 0) (:hscroll win 0)))]
                  (t/move (+ (:row rect) row-in-win) (+ (:col rect) col-in-win))))))
          (t/show-cur))
        (let [cur-win (get @rendered cur-path)]
          {:sig sig
           :cur-scroll (:scroll cur-win)
           :cur-hscroll (:hscroll cur-win)
           :dividers divs
           :viewports (into {} (map (fn [[path w]]
                                      [path (assoc (select-keys w [:buffer :scroll :hscroll :gutter-width :folds :version])
                                                   :rect (get layouts path))]))
                            @rendered)
           :font-lock-updates @cache-ups})))))
