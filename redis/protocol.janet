# This file is an attempt at a streaming codec for the redis protocol.

# This is the default buffer size used for pre-allocating buffers when decoding
# responses. It is arbitrarily chosen, and at the time of writing, no benchmarks
# or other sorts of profiling has been done to determine whether it is a good
# number or not.
(def default-buffer-size 256)

# Here's ASCII constants for the named characters. They are used in the protocol
# to signal types, lengths and delimitations of data.
(def ASTERISK 42)
(def DOLLAR 36)
(def COLON 58)
(def MINUS 45)
(def PLUS 43)
(def CR 13)
(def LF 10)

# This variable is exported (leaky, but yeah.) so e.g. tests can enable
# assertions.
(var DEBUG false)
(defmacro assertpp
  "This is a helper macro that asserts the form (ie. that it is not falsy)
and pretty prints it in the error message."
  [form]
  (if DEBUG
    ~(assert ,form (string/format "Form %M was nil" ',form))
      form))

(defn read-number
  "Decodes the next number from the stream. Returns a tuple where the first
element is the news index and the second is the decoded number. If debug is
enabled, will assert that the element is a number."
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
  "Note: the contents of `buf` are not to be used. It will not contain
the full message, but will contain jibberish."
  [stream &opt buf]
  (default buf (buffer/new default-buffer-size))
  (var index 0)
  (var array-value nil)
  (var simple-value nil)
  (var is-error false)
  (while true
    (let [c (:chunk stream 1 buf)]
      (when (nil? c) (break)) # end of stream, it seems

      # Here we dispatch on the next character. The redis protocol defines
      # a couple of special chars (that we have defined the ASCII byte values
      # for above) that we check and dispatch on.
      (case (in buf index)

        # A plus indicates a simple string
        PLUS (let [val (read-simple-string stream buf (inc index))]
               (set simple-value val)
               (break))

        # A minus is like a simple string, but indicates that there was
        # an error. Otherwise nothing special.
        MINUS (let [val (read-simple-string stream buf (inc index))]
                (set is-error true)
                (set simple-value val)
                (break))

        # An asterisk indicates an sequence of values. Each element could be
        # any sort of value, theoretically, but I think the protocol dictates
        # that element are other type than sequences. Don't quote me on that tho.
        ASTERISK (let [[new-index len] (read-number stream buf (inc index))]
                   (set array-value @[])
                   (set index new-index))

        # A colon indicates a number.
        COLON (let [[new-index numbr] (read-number stream buf (inc index))]
                (set index new-index)
                (if array-value
                  (array/push array-value numbr)
                  (do
                    (set simple-value numbr)
                    (break))))

        # A dollar indicates binary data of arbitrary length.
        DOLLAR (let [[new-index len] (read-number stream buf (inc index))]
                 (set index new-index)
                 (if (= len -1) # if the length is -1, that indicates nil
                   (array/push array-value nil)
                   (let [_ (:chunk stream 1 buf) # discard the remaining LF
                         value (read-string stream buf len)]
                     (array/push array-value (string value)))))
        # Default, just increment index
        (++ index))
      ))
  {:is-error is-error
   :data (or array-value
             simple-value)})
