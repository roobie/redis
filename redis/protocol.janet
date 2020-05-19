(def- sep "\r\n")

# @"+OK\r\n"
# @"$5\r\nthere\r\n"
# @"-ERR wrong number of arguments for 'hdel' command"
(defn decode [redis-message]
  (def ok-pattern ~(* "+OK" ,sep -1))
  (def err-pattern ~(* "-" (<- (some (if-not ,sep 1))) ,sep -1))
  (def data-pattern ~(* "$" (<- :d+) ,sep))
  (or
    (when-let [result (peg/match err-pattern redis-message)]
      [:error result])
    (when-let [result (peg/match ok-pattern redis-message)]
      [:ok])
    (when-let [result (peg/match data-pattern redis-message)]
      (do
        (def [len] result)
        (def start (+ (length "$") (length sep) (length len)))
        (def message (buffer/slice redis-message start (+ start (scan-number len))))
        [:data message]))
    # (pp redis-message)
    )
  )

(defn encode [& rest]
  (defn strlen [v] (string (length v)))
  # TODO not pretty. Would be nice with a recursive function
  (var buf (buffer "*" (strlen rest)))
  (each item rest
    (set buf (buffer buf sep "$" (strlen item) sep (string item))))
  (string buf sep))
