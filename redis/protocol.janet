
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
