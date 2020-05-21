
## Based on the spork/msg implementation

(defn make-recv
  [stream &opt unpack]
  (def buf @"")
  (default unpack string)
  (fn receiver []
    (buffer/clear buf)
    (if-not (:chunk stream 4 buf) (break))
    (def [b0 b1 b2 b3] buf)
    (def len (+ b0 (* b1 0x100) (* b2 0x10000) (* b3 0x1000000)))
    (buffer/clear buf)
    (if-not (:chunk stream len buf) (break))
    (pp [:read buf])
    (unpack (string buf))))

(defn make-send
  [stream &opt pack]
  (def buf @"")
  (default pack string)
  (fn sender [msg]
    (def x (pack msg))
    (buffer/clear buf)
    (buffer/push-word buf (length x))
    (buffer/push-string buf x)
    (pp [:write buf])
    (:write stream buf)
    nil))

(def default-buffer-size 256)
(def ASTERISK 42)
(def DOLLAR 36)
(def COLON 58)
(def MINUS 45)
(def PLUS 43)
(def CR 13)
(def LF 13)

(def DEBUG true)
(defmacro assertpp
  [form]
  (if DEBUG
    ~(assert ,form (string/format "Form %M was nil" ',form))
    form))

(defn read-number
  [stream buf start-index]
  (var index start-index)
  (var num 0)
  (while true
    (:chunk stream 1 buf)
    (when (= CR (in buf index))
      (let [numend index]
        (set index (inc index))
        (set num (assertpp
                  (scan-number (buffer/slice buf start-index numend))))
        (break)))
    (set index (inc index)))
  [index num])

(defn read-string
  [stream buf value-length]
  (def valbuf (buffer/new value-length))
  (:chunk stream value-length valbuf)
  valbuf)

(defn read-simple-string
  [stream buf start-index]
  (var index start-index)
  (var numend start-index)
  (while true
    (:chunk stream 1 buf)
    (when (= CR (in buf index))
      (do
        (set numend index)
        (set index (inc index))
        (break)))
    (set index (inc index)))
  (string (buffer/slice buf start-index numend)))

(defn decode-stream
  "Note: the contents of `buf` are not to be used. It will not contain the full message."
  [stream &opt buf]
  (default buf (buffer/new default-buffer-size))
  (var index 0)
  (def state @{:array-length nil :items nil :simple-value nil})
  (while true
    (let [c (:chunk stream 1 buf)]
      (when (nil? c) (break))
      (case (in buf index)
        PLUS (let [val (read-simple-string stream buf (inc index))]
               (put state :simple-value val)
               (break))
        MINUS (let [val (read-simple-string stream buf (inc index))]
               (put state :simple-value val)
               (break))
        ASTERISK (let [[new-index len] (read-number stream buf (inc index))]
                   (put state :items @[])
                   (set index new-index)
                   (put state :array-length len))
        COLON (let [[new-index numbr] (read-number stream buf (inc index))]
                (set index new-index)
                (if-let [items (get state :items)]
                  (array/push items numbr)
                  (do
                    (put state :simple-value numbr)
                    (break))))
        DOLLAR (let [[new-index len] (read-number stream buf (inc index))]
                 (set index new-index)
                 (if (= len -1)
                   (array/push (in state :items) nil)
                   (let [_ (:chunk stream 1 buf) # discard the remaining LF
                         value (read-string stream buf len)]
                     (array/push (in state :items) (string value)))))
        # Default, just increment index
        (set index (inc index)))
      ))
  state)

(comment
 (def numstart (inc index))
 (while true
   (:chunk stream 1 buf)
   (when (= CR (in buf index))
     (let [numend index]
       (set index (inc index))
       (def num (assertpp
                 (scan-number (buffer/slice buf numstart numend))))
       (put state :array-length num)
       (break)))
   (set index (inc index)))
 )
