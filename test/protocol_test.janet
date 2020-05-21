# https://redis.io/topics/clients

(import spork/msg)
(import ../redis/protocol :as p)


(def host "127.0.0.1")
(def port "45679")

# similar to/inspired by spork/rpc/server
(defn server
  [&opt host port]
  (net/server
   host port
   (fn on-connection
     [stream]
     (def buf @"")
     (defer (:close stream)
       (def recv (msg/make-recv stream))
       (def send (msg/make-send stream))
       (print "Client connected")
       (while (def msg (recv))
         (send "OK")))
     (print "Closing connection"))))

(def srv (server host port))

(defer (:close srv)
  (def client (net/connect host port))
  (def recv (msg/make-recv client))
  (def send (msg/make-send client))
  (send "asdf")
  (pp (recv))
  (send "asdf")
  (pp (recv))

  (def client2 (net/connect host port))
  (def recv2 (msg/make-recv client))
  (def send2 (msg/make-send client))

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
