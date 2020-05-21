(declare-project
 :name "redis"
 :description "A library for communicating with redis over TCP."
 :author "Björn Roberg"
 :license "MIT"
 :url "https://github.com/roobie/redis"
 :repo "git+https://github.com/roobie/redis.git")

(declare-source :source ["redis"])

(phony "watch" []
       (do
         (os/shell "fswatch -o . | xargs -n1 -I{} ./watch")))
