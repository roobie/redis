
#(import ./commands :export true)

(import ./protocol)

(defmacro redis-command
  [name arguments &keys {:flags flags}]
  (with-syms [$result]
    ~(defn ,name ,(tuple 'stream ;arguments)
       # (net/write stream (protocol/encode :values ',name ,;arguments))
       (let [x (protocol/encode :values ',name ,;arguments)]
         (pp [:command ,name x])
         (:write stream x))
       (def ,$result (protocol/decode-stream stream))
       ,$result)))

(redis-command SET [key value])
(redis-command GET [key])
(redis-command HSET [key field value]) # ... [field value]
(redis-command HGET [key field])
(redis-command HDEL [key field])

## more to come...
