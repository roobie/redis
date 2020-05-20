
(def- sep "\r\n")
(def bufsz 0xff)

# @"+OK\r\n"
# @"$5\r\nthere\r\n"
# @"-ERR wrong number of arguments for 'hdel' command"

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
      (error result))
    (when-let [result (peg/match ok-pattern redis-message)]
      :ok)
    (when-let [result (peg/match array-pattern redis-message)]
      (decode-array redis-message))
    (when-let [result (peg/match string-pattern redis-message)]
      (decode-string redis-message))
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
    :values (do
              (var buf (buffer "*" (strlen rest)))
              (each item rest
                (set buf (buffer buf sep "$" (strlen item) sep (string item))))
              (string buf sep))))

(defn read-to-end [stream]
  (def result (net/read stream bufsz))
  (if (= (length result) bufsz)
    (buffer result (read-to-end stream))
    result))
