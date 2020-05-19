(import redis :as r)


(def redis (net/connect "127.0.0.1" 6379))
(def result (r/SET redis "hello" "there"))
(pp result)
(pp (r/GET redis "hello"))
(pp (r/HDEL redis "hello" "f"))

(match (r/GET redis "hello")
  [:data d] (pp d)
  [:error e] (pp e))

(def really-long-string "ABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD")

(match (r/SET redis "a" really-long-string)
  [:ok] (print "OK")
  [:data d] (print d)
  [:error e] (printf "Error! %s" e))

(match (r/GET redis "a")
  [:ok] (print "OK")
  [:data d] (print d)
  [:error e] (printf "Error! %s" e))
