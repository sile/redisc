(defpackage :redisc
  (:use :common-lisp)
  (:shadow :common-lisp close set get sort type
                        append)
  (:export connect
           close
           
           *varsion* ;; version of redis
           *default-connection* 

           ;; command: key
           del exists expire expireat keys
           move object persist pexpire pexpireat
           pttl randomkey rename renamenx sort
           ttl type

           ;; command: string
           append decr decrby get getbit
           getrange getset incr incrby incrbyfloat
           mget mset msetnx psetex set setbit
           setex setnx setrange strlen
           
           ;; command: hash
           hdel hexists hget hgetall
           hincrby hincrbyfloat hkeys hlen 
           hmget hmset hset hsetnx hvals

           ;; command: list
           blpop brpop brpoplpush lindex linsert
           llen lpop lpush lpushx lrange
           lrem lset ltrim rpop rpoplpush
           rpush rpushx

           ;; command: set
           sadd scard sdiff sdiffstore
           sinter sinterstore sismember smembers
           smove spop srandmember srem 
           sunion sunionstore

           ;; command sorted set
           zadd zcard zcount zincrby
           zinterstore zrange zrangebyscore
           zrank zrem zremrangebyrank zremrangebyscore zrevrange
           zrevrangebyscore zrevrank zscore zunionstore
           
           ;; command Pub/Sub
           psubscribe publish punsubscribe
           subscribe unsubscribe
           
           ;; command transaction
           discard exec multi unwatch watch
           ))
(in-package :redisc)

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(vector octet))

(defparameter *delim* 
  (map 'octets #'char-code '(#\Return #\Newline)))

;; TODO: pipeline対応
;; (pipe (set ..) (get ...) (expire ...))