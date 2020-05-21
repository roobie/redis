# See https://redis.io/topics/protocol

(def- sep "\r\n")
(def bufsz 0xffff)

(defn decode-string
  [str]
  (def strcapt (peg/match ~(* "$" (<- :d+) ,sep ($)) str))
  (def strlen (assert (scan-number (in strcapt 0))))
  (def pos (in strcapt 1))
  (buffer/slice str pos (+ pos strlen)))

(defn decode-array
  [str]
  (def captures (peg/match ~(* "*" (<- :d+) ,sep ($)) str))
  (def alen (assert (scan-number (in captures 0))))
  (var pos (in captures 1))
  (def results @[])
  (for i 0 alen
    (def strcapt (peg/match ~(* ,pos "$" (<- :d+) ,sep ($)) str))
    (def strlen (assert (scan-number (in strcapt 0))))
    (set pos (in strcapt 1))
    (array/push results (buffer/slice str pos (+ pos strlen)))
    (set pos (+ (in strcapt 1) strlen (length sep))))
  results)

(defn decode [redis-message]
  (def ok-pattern ~(* "+OK" ,sep -1))
  (def err-pattern ~(* "-" (<- (some (if-not ,sep 1))) ,sep -1))
  (def string-pattern ~(* "$" :d+ ,sep))
  (def integer-pattern ~(* ":" (<- :d+) ,sep -1))
  (def array-pattern ~(* "*" :d+ ,sep))

  (or
    (when-let [result (peg/match err-pattern redis-message)]
      [:error ;result])
    (when-let [result (peg/match ok-pattern redis-message)]
      :ok) # should this also be a tuple (with one item)?
    (when-let [result (peg/match integer-pattern redis-message)]
      [:integer (assert (scan-number (in result 0)))])
    (when-let [result (peg/match array-pattern redis-message)]
      [:array (decode-array redis-message)])
    (when-let [result (peg/match string-pattern redis-message)]
      [:buffer (decode-string redis-message)])
    (error (string/format "Fall through! Don't know how to handle redis-message=%m" redis-message))
    ))

(defn strlen [v]
  "base 10 length of v as string"
  (string (length v)))

(defn encode-string [str]
  (buffer "$" (strlen str) sep (string str) sep))

(defn encode [what & rest]
  (match what
    :ok "+OK\r\n"
    :nil "$-1\r\n"
    :string (encode-string (in rest 0))
    :error (string "-" (encode :string (in rest 0)))
    :array (encode :values ;(get rest 0))
    :values (do
              # TODO don't create a lotta buffers here. Use one and mutate that.
              (var buf (buffer "*" (strlen rest) sep))
              (each item rest
                (set buf (buffer buf (encode-string item))))
              (string buf))
    _ (error (string/format "Fall through! Don't know how to handle %m" what))))

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
  [accumulator last])

(defn consume-assert= [stream expected]
  (printf "Assert next chars in stream: %m" expected)
  (let [val (string (:read stream (length expected)))]
    (assert (= val) (string/format "Expected %m but got %m" expected val))))

(defn decode-string-stream [stream &opt wrap]
  (pp [:decode-string-stream])
  (default wrap true)
  (def [numstr lastchar]
    (read-while stream (fn [c]
                         (peg/match '(% :d) c))))
  (def num (assert (scan-number numstr)))

  (assert (= "\r" (string lastchar)) (string/format "expected \\r lastchar: %q" lastchar))
  (consume-assert= stream "\n")

  (def accumulator (:read stream num))

  (consume-assert= stream "\r\n")

  (if wrap
   [:buffer accumulator]
   accumulator))

(defn decode-array-stream [stream]
  (pp [:decode-array-stream])
  (def [numstr lastchar] (read-while stream (fn [c]
                                              (peg/match '(% :d) c))))
  (def num (assert (scan-number numstr)))
  (assert (= "\r" (string lastchar)) (string/format "lastchar: %q" lastchar))
  (consume-assert= stream "\n")

  (def accumulator @[])
  (for i 0 num
    (assert (= "$" (string (net/read stream 1))))
    ## the call to decode-string-stream should consume the last \r\n
    (array/push accumulator (decode-string-stream stream false)))

  [:array accumulator])

(defn decode-ok-stream [stream]
  (pp [:decode-ok-stream])
  (consume-assert= stream "OK\r\n")
  :ok)
(defn decode-error-stream [stream]
  (pp [:decode-error-stream])
  (def [reststr lastchar]
    (read-while stream (fn [c]
                         (peg/match '(% (some (if-not "\r" 1))) c))))
  ## eat up the remaining delimiting chars
  (assert (= "\r" (string lastchar)))
  (consume-assert= stream "\n")

  [:error (string reststr)])

(defn decode-integer-stream [stream]
  (pp [:decode-integer-stream])
  (def [numstr lastchar] (read-while stream (fn [c]
                                            (peg/match '(% :d) c))))
  (assert (= "\r" (string lastchar)))
  (consume-assert= stream "\n")
  [:integer (assert (scan-number numstr))])

(defn decode-stream [stream]
  (let [fst (net/read stream 1)]
    (case (string fst)
      "*" (decode-array-stream stream)
      "$" (decode-string-stream stream)
      "+" (decode-ok-stream stream)
      "-" (decode-error-stream stream)
      ":" (decode-integer-stream stream)
      (do
        (if (nil? fst)
          nil
          (do
        (def [reststr lastchar]
          (read-while
           stream (fn [c]
                    (if (nil? c) nil
                        (peg/match ~(% (some (if-not "\r" 1))) c)))))
        (pp [:unknown reststr lastchar])
        #(assert (= "\r" (string lastchar)))
        #(consume-assert= stream "\n")
        (error (string/format "unexpected input: %q" fst))
        )))))
  )
