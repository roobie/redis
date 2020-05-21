# See https://redis.io/topics/protocol

(def- sep "\r\n")
(def bufsz 0xffff)

(defn decode-string
  [str]
  (def strcapt (peg/match ~(* "$" (<- :d+) ,sep ($)) str))
  (def strlen (scan-number (in strcapt 0)))
  (def pos (in strcapt 1))
  (buffer/slice str pos (+ pos strlen)))

(defn decode-array
  [str]
  (def captures (peg/match ~(* "*" (<- :d+) ,sep ($)) str))
  (def alen (scan-number (in captures 0)))
  (var pos (in captures 1))
  (def results @[])
  (for i 0 alen
    (def strcapt (peg/match ~(* ,pos "$" (<- :d+) ,sep ($)) str))
    (def strlen (scan-number (in strcapt 0)))
    (set pos (in strcapt 1))
    (array/push results (buffer/slice str pos (+ pos strlen)))
    (set pos (+ (in strcapt 1) strlen (length sep)))
    )
  results)

(defn decode [redis-message]
  (def ok-pattern ~(* "+OK" ,sep -1))
  (def err-pattern ~(* "-" (<- (some (if-not ,sep 1))) ,sep -1))
  (def string-pattern ~(* "$" :d+ ,sep))
  (def array-pattern ~(* "*" :d+ ,sep))

  (or
    (when-let [result (peg/match err-pattern redis-message)]
      [:error ;result])
    (when-let [result (peg/match ok-pattern redis-message)]
      :ok)
    (when-let [result (peg/match array-pattern redis-message)]
      [:array (decode-array redis-message)])
    (when-let [result (peg/match string-pattern redis-message)]
      [:buffer (decode-string redis-message)])
    (error "fall through")
    )
  )

(defn encode [what & rest]
  (defn strlen [v] (string (length v)))
  # TODO not pretty. Would be nice with a recursive function
  (match what
    :ok "+OK\r\n"
    :string (do # TODO move to own func
              (def item (in rest 0))
              (buffer "$" (strlen item) sep (string item) sep))
    :error (string "-" (encode :string (in rest 0)))
    :array (encode :values ;(get rest 0))
    :values (do
              (var buf (buffer "*" (strlen rest)))
              (each item rest
                (set buf (buffer buf sep "$" (strlen item) sep (string item))))
              (string buf sep))
    _ (error "fall-through")))

(defn read-while [stream func &opt chunk-size]
  (default chunk-size 1)
  (var accumulator @"")
  (var last "")
  (while (let [nxt (:read stream chunk-size)
               ok (func nxt)]
           (set last nxt)
           (when ok
             (buffer/push-string accumulator (string nxt)))
           ok)
    0)
  [accumulator last]
  )

(defn decode-string-stream [stream &opt wrap]
  (default wrap true)
  (def [numstr lastchar] (read-while stream (fn [c]
                                              (peg/match ~(% :d) c))))
  (def num (scan-number numstr))
  (assert num)
  (assert (= "\r" (string lastchar)) (string/format "lastchar: %q" lastchar))
  (let [nextc (string (net/read stream 1))]
    (assert (= "\n" nextc) (string/format "expected \\n, got %q" nextc))
    )
  (def accumulator (:read stream num))
  (assert (= "\r" (string (:read stream 1))) (string/format "lastchar: %q" lastchar))
  (let [nextc (string (:read stream 1))]
    (assert (= "\n" nextc) (string/format "expected \\n, got %q" nextc))
    )
  (if wrap
   [:buffer accumulator]
   accumulator)
  )
  #accumulator)
  # [:buffer accumulator])

(defn decode-array-stream [stream]
  (def [numstr lastchar] (read-while stream (fn [c]
                                              (peg/match ~(% :d) c))))
  (def num (scan-number numstr))
  (assert num)
  (assert (= "\r" (string lastchar)) (string/format "lastchar: %q" lastchar))
  (let [nextc (string (net/read stream 1))]
    (assert (= "\n" nextc) (string/format "expected \\n, got %q" nextc)))
  (def accumulator @[])
  (for i 0 num
    (assert (= "$" (string (net/read stream 1))))
    (array/push accumulator (decode-string-stream stream false))
    )
  # accumulator)
  [:array accumulator])

(defn decode-ok-stream [stream]
  (assert (= "O" (string (net/read stream 1))))
  (assert (= "K" (string (net/read stream 1))))
  (assert (= "\r" (string (net/read stream 1))))
  (assert (= "\n" (string (net/read stream 1))))
  :ok)
(defn decode-error-stream [stream]
  (def [reststr lastchar]
    (read-while stream (fn [c]
                         (peg/match ~(% (some (if-not "\r" 1))) c))))
  ## eat up the remaining delimiting chars
  (assert (= "\r" (string lastchar)))
  (assert (= "\n" (string (:read stream 1))))

  [:error (string reststr)])

(defn decode-integer-stream [stream]
  (def [numstr lastchar] (read-while stream (fn [c]
                                            (peg/match ~(% :d) c))))
  (assert (= "\r" (string lastchar)))
  (assert (= "\n" (string (:read stream 1))))
  [:integer (scan-number numstr)])

(defn decode-stream [stream]
  (let [fst (net/read stream 1)]
    (case (string fst)
      "*" (decode-array-stream stream)
      "$" (decode-string-stream stream)
      "+" (decode-ok-stream stream)
      "-" (decode-error-stream stream)
      ":" (decode-integer-stream stream)
      (do
        (def [reststr lastchar]
          (read-while stream (fn [c]
                               (peg/match ~(% (some (if-not "\r" 1))) c))))
        (pp [reststr lastchar])
        (assert (= "\r" (string lastchar)))
        (assert (= "\n" (string (:read stream 1))))
        (error (string/format "unexpected input: %q" fst))
        )))
  )
