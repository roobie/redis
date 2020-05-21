# https://redis.io/topics/clients

(import spork/msg)
(import ../redis/protocol :as p)


(def host "127.0.0.1")
(def port "45679")

(comment
# similar to/inspired by spork/rpc/server
(defn server
  [&opt host port]
  (net/server
   host port
   (fn on-connection
     [stream]
     (def buf @"")
     (defer (:close stream)
       (def recv (p/make-recv stream))
       (def send (p/make-send stream))
       (print "Client connected")
       (while (def msg (recv))
         (send "OK")))
     (print "Closing connection"))))

(def srv (server host port))

(defer (:close srv)
  (def client (net/connect host port))
  (def recv (p/make-recv client))
  (def send (p/make-send client))
  (send "asdf")
  (pp (recv))
  (send "asdf")
  (pp (recv))

  (def client2 (net/connect host port))
  (def recv2 (p/make-recv client))
  (def send2 (p/make-send client))

  (send2 "asdf")
  (pp (recv2))

  (send "asdf")
  (pp (recv))
  (send2 "asdf")
  (pp (recv2))
  (send "asdf")
  (pp (recv))

  (:close client)
  (:close client2)
  )


# (defn handler [stream]
#   (let [recv (p/make-recv stream)
#         send (p/make-send stream)
#         buf @""]
#     (defer (:close stream)
#       # (defn looper []
#         (while true
#           (if-let [msg (recv)]
#             (send (string "OK " msg)))
#           )
#       #)
#       )
#     (print "Closing client")
#     )
#   )

# (def srv (net/server host port handler))

# (defer (:close srv)
#   (let [clt (net/connect host port)
#         recv (p/make-recv clt)
#         send (p/make-send clt)]
#     (defer (:close clt)
#       # (send "1")
#       # (assert (= "OK" (recv)))
#       # (send "1")
#       # (assert (= "OK" (recv)))
#       # (send "1")
#       # (assert (= "OK" (recv)))
#       (send "1")
#       (pp (recv))
#       (send "1")
#       (pp (recv))
#       )))

)

(def srv (net/server
          host port (fn handler
                      [stream]
                      (print "==")
                      (def buf (buffer/new 8))
                      (pp [:retval (p/decode-stream stream buf)])
                      (pp [:mybuf buf])
                      (print "==")
                      )))

(defn send-command [cmd]
  (def client (net/connect host port))
  (defer (:close client)
    (:write client cmd)))
(defer (:close srv)
  (send-command "*3\r\n$-1\r\n:1\r\n$2\r\nAB\r\n")
  (send-command "+OK\r\n")
  (send-command "-ERR stuff did not go well\r\n")
  (send-command ":40282\r\n")
  )
