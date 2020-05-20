
#(import ./commands :export true)

(import ./protocol)

(defmacro redis-command
  [name arguments &keys {:flags flags}]
  (with-syms [$result]
    ~(defn ,name ,(tuple 'stream ;arguments)
       # (net/write stream (protocol/encode :values ',name ,;arguments))
       (let [x (protocol/encode :values ',name ,;arguments)]
         (net/write stream x))
       (def ,$result (protocol/decode-stream stream))
       ,$result)))

(redis-command SET [key value])
(redis-command GET [key])
(redis-command HSET [key field value]) # ... [field value]
(redis-command HDEL [key field])

## more to come...
