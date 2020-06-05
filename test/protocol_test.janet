# https://redis.io/topics/clients

(import ../redis/protocol :as p)

(set p/DEBUG true)

(def host "127.0.0.1")
(def port "45679")

(defn run-1
  [data form]
  (def srv
    (net/server
     host port
     (fn handler
       [stream]
       (def buf (buffer/new 8))
       (def command (p/decode-stream stream buf))
       (assert (deep= command form)))))

  (def client (net/connect host port))
  (:write client data)
  (:close client)
  (:close srv))

(run-1
 "*3\r\n$-1\r\n:1\r\n$2\r\nAB\r\n"
 {:is-error false :data @[nil 1 "AB"]})
(run-1
 "+OK\r\n"
 {:is-error false :data "OK"})
(run-1
 "-ERR stuff did not go well\r\n"
 {:is-error true :data "ERR stuff did not go well"})
(run-1
 ":40282\r\n"
 {:is-error false :data 40282})

(do
  (def strlen 1024)
  (def str (string/repeat "A" strlen))
  (run-1
   (string "*1\r\n$"strlen"\r\n"str"\r\n")
   {:is-error false :data @[str]}))
