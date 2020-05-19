# redis
A redis library for Janet

## Example

```janet
(import redis :as r)

(def redis (try
             (net/connect "127.0.0.1" 6379)
             ((err)
              (print "redis needs to be running on the default port for tests to run")
              (os/exit 1))))

(def k0 "k0")
(def v0.0 "0")
(var result (r/SET redis k0 v0.0))
(assert (= :ok (in result 0)))
(set result (r/GET redis k0))
(assert (= v0.0 (string (in result 1))))

(match (r/GET redis k0)
  [:data d] :ok
  [:error e] (error "Test GET failed"))

(def really-long-string "ABCD\r\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\rBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB\nCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\f\f\r\n\rDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD")


(def k1 "k1")
(match (r/SET redis k1 really-long-string)
  [:ok] :ok
  [:data d] (error (string d))
  [:error e] (error (string e)))

(match (r/GET redis k1)
  [:ok] (error "Test GET failed")
  [:data d] (assert (= really-long-string (string d)))
  [:error e] (error (string e)))
```
