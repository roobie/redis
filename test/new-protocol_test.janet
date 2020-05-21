
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
    (:write stream buf)
    nil))

(def host "127.0.0.1")
(def port "45679")
(defn handler [stream]
  (let [recv (make-recv stream)
        send (make-send stream)]
    (defer (:close stream)
      (def input (recv))
      (send "OK"))))
(def srv (net/server host port handler))
(defer (:close srv)
  (let [clt (net/connect host port)
        recv (make-recv clt)
        send (make-send clt)]
    (defer (:close clt)
      (send "1")
      (assert (= "OK" (recv))))))
