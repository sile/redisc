(defpackage :redisc
  (:use :common-lisp)
  (:shadow :common-lisp close ;set get sort type
                        ;append time 
           apropos apropos-list
           )
  (:export connect close
           with-connect with-default
           q q* q@ q!
           multi multi* multi@ 
           pipe pipe* pipe@
           with-watch 
           listen-message

           apropos
           apropos-list
           desc

           *default-connection*

           *varsion* ;; version of redis client
           )
  
  #+C
  (:export *varsion* ;; version of redis
           

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

           ;; scripting
           
           ;; connection
           auth echo ping quit select

           ;; server
           bgrewriteaof bgsave config-get config-set config-resetstat
           dbsize debug-object debug-segfault flushall flushdb 
           info lastsave #|monitor|# save shutdown slaveof slowlog sync time
           ))
(in-package :redisc)

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(vector octet))

(defparameter *delim* 
  (map 'octets #'char-code '(#\Return #\Newline)))

(deftype exec-mode () '(member :single :pipe :multi :watch))

;; TODO: 別のファイルに移動
(defstruct connection
  (socket    t :type usocket:stream-usocket)
  (exec-mode t :type exec-mode))

;; TODO: 作成日時やドキュメントのバージョンを残しておく