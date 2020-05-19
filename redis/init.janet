
#(import ./commands :export true)

(import ./protocol)

(defmacro redis-command
  [name arguments &keys {:flags flags}]
  (with-syms [$result]
    ~(defn ,name ,(tuple 'stream ;arguments)
       (net/write stream (protocol/encode ',name ,;arguments))
       (def ,$result ((fn read-to-end []
                        (def bufsz 0xff)
                        (def result (net/read stream bufsz))
                        (if (= (length result) bufsz)
                          (buffer result (read-to-end))
                          result))))
       (protocol/decode ,$result))))

(redis-command SET [key value])
(redis-command GET [key])
(redis-command HSET [key field value]) # ... [field value]
(redis-command HDEL [key field])

## more to come...
