(import ../redis :as r)

(import ../redis/protocol :as p)

(let [a (p/decode-string "$4\r\nABCD\r\n")]
  (assert (= "ABCD" (string a))))

(let [a (p/decode-array "*2\r\n$4\r\nABCD\r\n$8\r\nABCDEFGH\r\n")]
  (assert (deep= @["ABCD" "ABCDEFGH"] (map string a))))

(let [ok (p/decode "+OK\r\n")
      [eok err] (protect (p/decode "-ERR asdf\r\n"))
      stringres (p/decode "$5\r\nABCDE\r\n")
      arrayres (p/decode "*2\r\n$0\r\n\r\n$1\r\nA\r\n")]
  (assert (= :ok ok) "should be ok")
  (assert (deep= [true [:error "ERR asdf"]] [eok err]) "should be error")
  (let [[k v] stringres]
    (assert (= :buffer k) "should be buffer")
    (assert (= "ABCDE" (string v)) "should be buffer value"))
  (let [[k v] arrayres]
    (assert (= :array k))
    (assert (deep= @["" "A"] (map string v)))
    (assert (= "" (string (in v 0))))
    (assert (= "A" (string (in v 1))))))

(let [okstring (p/encode :ok)
      errstring (p/encode :error "ERR 1")
      strstring (p/encode :string "asdf")
      arrstring (p/encode :values "" "A")
      arrstring2 (p/encode :array ["" "A"])
     ]
  (assert (= "+OK\r\n" (string okstring)))
  (assert (= "-$5\r\nERR 1\r\n" (string errstring)))
  (assert (= "$4\r\nasdf\r\n" (string strstring)))
  (assert (= "*2\r\n$0\r\n\r\n$1\r\nA\r\n" arrstring))
  (assert (= "*2\r\n$0\r\n\r\n$1\r\nA\r\n" arrstring2)))

(import spork/msg)
(do
  (def host "127.0.0.1")
  (def port "45678")
  (def srv (net/server host port
              (fn handler [conn]
                (defer (:close conn)
                  (print "srv.1")
                  (def input (p/decode-stream conn))
                  (pp ["srv.2" input])
                  (var result nil)
                  #(assert (= :buffer (in input 0)))
                  #(if (= "Hello" (string (in input 1)))
                  (if (= "Hello" (string (in input 1)))
                    (set result (p/encode :ok))
                    (set result (p/encode :error "invalid")))
                  (:write conn result)
                  (pp ["srv.3" result])
                  (def input2 (p/decode-stream conn))
                  (pp ["srv.4" input2])
                  (assert (deep= @["SET" "key1" "value1"] (map string (in input2 1))))
                  (:write conn (p/encode :ok))
                  ))))


  (def client (net/connect host port))
  (defer (:close client)
    (print "cli.1")
    (:write client (p/encode :string "Hello"))
    (print "cli.2")
    (let [result (p/decode-stream client)]
      (print "cli.3")
      (assert (= :ok result)))
    (:write client (p/encode :values "SET" "key1" "value1"))
    (let [result (p/decode-stream client)]
      (pp ["cli.4" result])
      (assert (= :ok result))
      )
    (:close srv)
  ))


#(comment
 (def redis (try
              (net/connect "127.0.0.1" 6379)
              ((err)
               (print "redis needs to be running on the default port for tests to run")
               (os/exit 1))))
 (defer (:close redis)
   (def k0 "k0")
   (def v0.0 "0")
   (var result (r/SET redis k0 v0.0))
   (assert (= :ok result))
   (set result (r/GET redis k0))
   (pp ["get.1" result])
   (let [[typ val] result]
     (assert (= :buffer typ))
     (assert (= v0.0 (string val)))
     )

   (pp [:dbg (r/GET redis k0)])

   (match (r/GET redis k0)
     :error (error "Test GET failed")
     dat (assert (= v0.0 (string (in dat 1))))
     _ (error "fall through"))

   (def really-long-string "ABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD")


   # make key `a` be simple KV
   (r/SET redis "a" "1")
   (def k1 "k1")
   # so this should ERR
   (let [[typ val] (r/HSET redis "a" "b" "c")]
     (assert (= :error typ))
     (assert (= "WRONGTYPE Operation against a key holding the wrong kind of value" (string val)))
     )

   # however this should be integer
   (let [[typ val] (r/HSET redis "P" "Q" "X")]
     (assert (= :integer typ))
     (assert (= "0" (string val)))
     )

   (pp (r/HGET redis "P" "Q"))
   )

#)
