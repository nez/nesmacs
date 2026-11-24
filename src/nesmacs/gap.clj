(ns nesmacs.gap
  "Persistent text: an immutable char[] plus a sorted int[] of newline
   positions (lidx) for O(log n) line lookups.

   The namespace kept its historical `gap` name, but the gap is gone: every
   edit already allocated a fresh full-size array, so tracking gs/ge only
   added offset arithmetic and a three-way copy path. A plain char[] has the
   same O(n) edit cost with less to get wrong.")

;; --- Text deftype ---

(deftype Text [^chars buf ^ints lidx]
  CharSequence
  (length [_] (alength buf))
  (charAt [_ i] (aget buf (int i)))
  (subSequence [_ start end]
    (String. buf (int start) (int (- end start))))
  (toString [_] (String. buf))

  clojure.lang.Counted
  (count [_] (alength buf))

  Object
  (equals [this other]
    (cond
      (identical? this other) true
      (nil? other) false
      (instance? CharSequence other)
      (let [^CharSequence o other
            n (alength buf)]
        (and (= n (.length o))
             (loop [i 0]
               (if (>= i n) true
                 (if (= (aget buf (int i)) (.charAt o (int i)))
                   (recur (inc i))
                   false)))))
      :else false))
  (hashCode [_]
    (let [n (alength buf)]
      (loop [i 0 h (int 0)]
        (if (>= i n) (int h)
          (recur (inc i)
                 (unchecked-add-int
                   (unchecked-multiply-int h (int 31))
                   (int (aget buf (int i))))))))))

;; --- Line index ---

(defn- count-newlines ^long [^String s]
  (let [n (.length s)]
    (loop [i 0 c 0]
      (if (>= i n) c
        (recur (inc i) (if (= (.charAt s i) \newline) (inc c) c))))))

(defn- lidx-bound
  "First index in lidx whose value is >= v. Uses Arrays/binarySearch."
  ^long [^ints lidx ^long v]
  (let [i (java.util.Arrays/binarySearch lidx (int v))]
    (if (neg? i) (dec (- i)) i)))

(defn- splice-lidx
  "New int[] reflecting logical edit of [from,to) → repl in a buffer whose
   current newline index is `lidx`. Drops entries in [from,to) (half-open —
   a newline at `from` is inside the replaced range and gone; one at `to`
   survives and shifts), appends newlines from `repl` at their new positions,
   shifts the tail by delta = rlen - (to - from)."
  ^ints [^ints lidx ^long from ^long to ^String repl]
  (let [n       (alength lidx)
        lo      (lidx-bound lidx from)
        hi      (lidx-bound lidx to)
        rlen    (.length repl)
        rnl     (count-newlines repl)
        delta   (- rlen (- to from))
        out-len (+ lo rnl (- n hi))
        out     (int-array out-len)]
    (when (pos? lo)
      (System/arraycopy lidx 0 out 0 lo))
    (when (pos? rnl)
      (loop [i 0 j lo]
        (when (< i rlen)
          (if (= (.charAt repl i) \newline)
            (do (aset out (int j) (int (+ from i)))
                (recur (inc i) (inc j)))
            (recur (inc i) j)))))
    (when (< hi n)
      (loop [i hi j (+ lo rnl)]
        (when (< i n)
          (aset out (int j) (int (+ (aget lidx i) delta)))
          (recur (inc i) (inc j)))))
    out))

(defn- scan-newlines
  "Build a sorted int[] of newline positions from a char array."
  ^ints [^chars buf]
  (let [n   (alength buf)
        nl  \newline
        cnt (loop [i 0 c 0]
              (if (>= i n) c
                (recur (inc i) (if (= (aget buf (int i)) nl) (inc c) c))))]
    (if (zero? cnt)
      (int-array 0)
      (let [arr (int-array cnt)]
        (loop [i 0 j 0]
          (when (< i n)
            (if (= (aget buf (int i)) nl)
              (do (aset arr (int j) (int i))
                  (recur (inc i) (inc j)))
              (recur (inc i) j))))
        arr))))

;; --- Constructors ---

(defn of
  "Create a Text from a String."
  ^Text [^String s]
  (let [n (.length s)
        a (char-array n)]
    (.getChars s 0 n a 0)
    (Text. a (scan-newlines a))))

;; --- Operations ---

(defn edit
  "Replace logical [from,to) with repl. Returns a new Text."
  ^Text [^Text t ^long from ^long to ^String repl]
  (let [b    (.-buf t)
        cn   (alength b)
        ;; Clamp to the buffer's actual range. Callers downstream of buf/edit
        ;; already normalize, but gap/edit is a public primitive and must not
        ;; walk off the end with a NegativeArraySizeException.
        from (-> from (max 0) (min cn))
        to   (-> to   (max from) (min cn))
        rn   (long (.length repl))
        nn   (+ (- cn (- to from)) rn)
        nb   (char-array nn)]
    (when (pos? from)
      (System/arraycopy b 0 nb 0 (int from)))
    (when (pos? rn)
      (.getChars repl 0 (int rn) nb (int from)))
    (let [rem (- cn to)]
      (when (pos? rem)
        (System/arraycopy b (int to) nb (int (+ from rn)) (int rem))))
    (Text. nb (splice-lidx (.-lidx t) from to repl))))

(defn substr
  "Extract logical [from,to) as a String. Clamps both endpoints into
   [0, count] so stale point/mark coords from a concurrently-shortened
   buffer don't trigger StringIndexOutOfBoundsException."
  ^String [^Text t ^long from ^long to]
  (let [b    (.-buf t)
        n    (alength b)
        from (-> from (max 0) (min n))
        to   (-> to   (max from) (min n))]
    (String. b (int from) (int (- to from)))))

;; --- Line index queries ---

(defn line-count
  "Number of lines (newlines + 1)."
  ^long [^Text t]
  (inc (alength (.-lidx t))))

(defn line-of
  "Zero-based line number containing pos. Returns the first lidx index i
   where lidx[i] >= pos (O(log n) via `Arrays/binarySearch`)."
  ^long [^Text t ^long pos]
  (lidx-bound (.-lidx t) pos))

(defn nth-line-start
  "Start position of zero-based line n."
  ^long [^Text t ^long n]
  (if (zero? n) 0
    (inc (aget (.-lidx t) (int (dec n))))))

(defn nth-line-end
  "End position of zero-based line n (position of \\n, or text length)."
  ^long [^Text t ^long n]
  (let [arr (.-lidx t)
        nl  (alength arr)]
    (if (< n nl)
      (long (aget arr (int n)))
      (long (alength (.-buf t))))))

(defn line-start
  "Start of line containing logical position pos."
  ^long [^Text t ^long pos]
  (nth-line-start t (line-of t pos)))

(defn line-end
  "End of line containing logical position pos."
  ^long [^Text t ^long pos]
  (nth-line-end t (line-of t pos)))