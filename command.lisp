(in-package :redisc)

(defparameter *default-connection* nil)

;; TODO: 良い仕組みを考える
(defun name-to-command (name &aux (name (symbol-name name)))
  (let ((p (position #\- name)))
    (if (null p)
        (list name)
      (list (subseq name 0 p) (subseq name (1+ p))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *command-list* '()))

(deftype command-category () '())
(deftype command-return-type () '())

(eval-when (:load-toplevel :compile-toplevel)
  (defstruct cmd
    name
    fn
    version
    category
    args
    return-type
    description))

(eval-when (:compile-toplevel :load-toplevel)
  (defun ensure-list (x)
    (if (atom x)
        (if (null x)
            x
          (list x))
      (cons (car x) (ensure-list (cdr x))))))

(defmacro defcmd (category version name args result-type description)
  (let ((connection (gensym))
        (vars (gensym))) ; XXX:
    `(progn 
       (push (make-cmd :name ,name
                       :version ,(symbol-name version)
                       :category ,category
                       :args ',args
                       :return-type ,result-type
                       :description ,description
                       :fn (lambda (,connection ,vars &key pipe) ;; TODO: nameが'xxx-yyy'形式の場合は、'xxx'部分をコマンド名にして、'yyy'は引数に付与する
                             (destructuring-bind ,args ,vars
                               (declare (ignore ,@(ensure-list args))))
                             (request ,connection ,name ,vars :pipe pipe))
                       )
             *command-list*))))

(defun get-cmd (name)
  (find name *command-list* :key #'cmd-name)) ; TODO: error-check

(defun execute* (name args &key (connection *default-connection*) pipe)
  (funcall (cmd-fn (get-cmd name)) connection args :pipe pipe))

(defun execute (name &rest args)
  (execute* name args))

(defun list-length-type (list)
  (cond ((null list)       :zero)
        ((null (cdr list)) :one)
        (t                 :multi)))
        
(defun q (commands &key (connection *default-connection*) timeout)
  (declare (ignore timeout))
  (let ((*default-connection* connection))
    (ecase (list-length-type commands)
      (:zero  (values nil t))
      (:one   (destructuring-bind ((name . args)) commands
                (multiple-value-bind (value ok) (execute name args)
                  (values (list value) ok))))
      (:multi 
       (execute :multi)
       (loop FOR (name . args) IN commands DO (execute* name args :pipe t))
       ;; TODO: 整理
       (let ((out (usocket:socket-stream connection)))
         (force-output out)
         (loop REPEAT (length commands)
               ;; TODO: 実行中に一つでもエラーになるコマンドがあれば、全体が失敗するようにする(バージョン不一致など)
               COLLECT (multiple-value-list (read-reply out))))
       (execute :exec)))))

(defmacro q* ((&key watch (connection *default-connection*) timeout) commands-exp)
  (let ((commands (gensym)))
    `(if (null ,watch)
         (q ,commands-exp :connection ,connection :timeout ,timeout)
       (progn
         (execute* :watch ,watch :connection ,connection) ; XXX: 多重評価
         (let ((,commands ,commands-exp))
           (ecase (list-length-type ,commands)
             (:zero (execute* :unwatch '() :connection ,connection))
             (:one  (execute* :multi '() :connection ,connection)
                    (q ,commands :connection ,connection :timeout ,timeout) ; TODO: 返り値確認
                    (execute* :exec '() :connection ,connection))
             (:multi (q ,commands :connection ,connection :timeout ,timeout))))))))

(defun q! (name &rest args)
  (execute* name args))

;; TODO: statusとtrueは怪しいので要チェック

;; keys
(defcmd :keys 1.0.0 :del (key . _) :integer "Delete a key")
(defcmd :keys 2.6.0 :dump (key) :string "Return a serialized version of the value stored at the specified key")
(defcmd :keys 1.0.0 :exists (key) :boolean "Determine if a key exists")
(defcmd :keys 1.0.0 :expire (key seconds) :boolean "Set a key's time to live in seconds")
(defcmd :keys 1.2.0 :expireat (key timestamp) :boolean "Set the expiration for a key as a UNIX timestamp")
(defcmd :keys 1.0.0 :keys (pattern) :list "Find all keys matching the given pattern")
(defcmd :keys 2.6.0 :migrate (host port key destinatino-db timeout) :status "Atomically transfer a key from a Redis instance to another one")
(defcmd :keys 1.0.0 :move (key db) :boolean "Move a key to another database")
(defcmd :keys 2.2.3 :object-refcount (key) :integer "Returns the number of references of the value associated with the specified key")
(defcmd :keys 2.2.3 :object-encoding (key) :string "Returns the kind of internal representation used in order to store the value associated with a key")
(defcmd :keys 2.2.3 :object-idletime (key) :integer "Returns the number of seconds since the object stored at the specified key is idle")
(defcmd :keys 2.2.0 :persist (key) :boolean "Remove the expiration from a key")
(defcmd :keys 2.6.0 :pexpire (key milliseconds) :boolean "Set a key's time to live in milliseconds")
(defcmd :keys 2.6.0 :pexpireat (key milliseconds-timestamp) :boolean "Set the expiration for a key as a UNIX timestamp specified in milliseconds")
(defcmd :keys 2.6.0 :pttl (key) :integer "Get the time to live for a key in milliseconds")
(defcmd :keys 1.0.0 :randomkey () :string "Return a random key from the keyspace")
(defcmd :keys 1.0.0 :rename (key newkey) :status "Rename a key")
(defcmd :keys 1.0.0 :renamenx (key newkey) :boolean "Rename a key, only if the new key does not exist")
(defcmd :keys 2.6.0 :restore (key ttl serialized-value) :status "Create a key using the provided serialized value, previosly obtained using DUMP")
(defcmd :keys 1.0.0 :sort (key . _) :list "Sort the elements in a list, set or sorted set")
(defcmd :keys 1.0.0 :ttl (key) :integer "Get the time to live for a key")
(defcmd :keys 1.0.0 :type (key) :status "Determine the type stored at key")

;; strings
(defcmd :strings 2.0.0 :append (key value) :integer "Append a value to a key")
(defcmd :strings 1.0.0 :decr (key) :integer "Decrement the integer value of a key by one")
(defcmd :strings 1.0.0 :decrby (key decrement) :integer "Decrement the integer value of a key by the given number")
(defcmd :strings 1.0.0 :get (key) :string "Get the value of a key")
(defcmd :strings 2.2.0 :getbit (key offset) :integer "Returns the bit value at offset in the string value stored at key")
(defcmd :strings 2.4.0 :getrange (key start end) :string "Get a substring of the string stored at a key")
(defcmd :strings 1.0.0 :getset (key value) :string "Set the string value of a key and return its old value")
(defcmd :strings 1.0.0 :incr (key) :integer "Increment the integer value of a key by one")
(defcmd :strings 1.0.0 :incrby (key increment) :integer "Increment the integer value of a key by the given amount")
(defcmd :strings 2.6.0 :incrbyfloat (key increment) :number "Increment the float value of a key by the given amount")
(defcmd :strings 1.0.0 :mget (key . _) :list "Get the values of all the given keys")
(defcmd :strings 1.0.1 :mset (key value . _) :true "Set multiple keys to multiple values")
(defcmd :strings 1.0.1 :msetnx (key value . _) :boolean "Set multiple keys to multiple values, only if none of the keys exist")
(defcmd :strings 2.6.0 :psetex (key milliseconds value) :true "Set the value and expiration in milliseconds of a key")
(defcmd :strings 1.0.0 :set (key value) :true "Set the string value of a key")
(defcmd :strings 2.2.0 :setbit (key offset value) :integer "Sets or clears the bit at offset in the string value stored at key")
(defcmd :strings 2.0.0 :setex (key seconds value) :true "Set the value and expiration of a key")
(defcmd :strings 2.2.0 :setrange (key offset value) :integer "Overwrite part of a string at key starting at the specified offset")
(defcmd :strings 2.2.0 :strlen (key) :integer "Get the length of the value stored in a key")

;; hashed
(defcmd :hashes 2.0.0 :hdel (key field . _) :integer "Delete one or more hash fields")
(defcmd :hashes 2.0.0 :hexists (key field) :boolean "Determine if a hash field exists")
(defcmd :hashes 2.0.0 :hget (key field) :string "Get the value of a hash field")
(defcmd :hashes 2.0.0 :hgetall (key) :list "Get all the fields and values in a hash")
(defcmd :hashes 2.0.0 :hincrby (key field increment) :integer "Increment the integer value of a hash field by the given number")
(defcmd :hashes 2.0.0 :hincrbyfloat (key field increment) :number "Increment the float value of a hash field by the given amount")
(defcmd :hashes 2.0.0 :hkeys (key) :list "Get all the fields in a hash")
(defcmd :hashes 2.0.0 :hlen (key) :integer "Get the number of fields in a hash")
(defcmd :hashes 2.0.0 :hmget (key field . _) :list "Get the values of all the given hash fields")
(defcmd :hashes 2.0.0 :hmset (key field value . _) :status "Set multiple hash fields to multiple values")
(defcmd :hashes 2.0.0 :hset (key field value) :boolean "Set the string value of a hash field")
(defcmd :hashes 2.0.0 :hsetnx (key field value) :boolean "Set the value of a hash field, only if the field does not exist")
(defcmd :hashes 2.0.0 :hvals (key) :list "Get all the values in a hash")

;; lists
(defcmd :lists 2.0.0 :blpop (key . _) :tuple2 "Remove and get the first element in a list, or block until one is available")
(defcmd :lists 2.0.0 :brpop (key . _) :tuple2 "Remove and get the last element in a list, or block until one is available")
(defcmd :lists 2.2.0 :brpoplpush (source destination timeout) :string "Pop a value from a list, push it to another list and return it; or block until one is available")
(defcmd :lists 1.0.0 :lindex (key index) :string "Get an element from a list by its index")
(defcmd :lists 2.2.0 :linsert (key before-or-after pivot value) :integer "Insert an element before or after another element in a list")
(defcmd :lists 1.0.0 :llen (key) :integer "Get the length of a list")
(defcmd :lists 1.0.0 :lpop (key) :string "Remove and get the first element in a list")
(defcmd :lists 1.0.0 :lpush (key value . _) :integer "Prepend one or multiple values to a list")
(defcmd :lists 2.2.0 :lpushx (key value) :integer "Prepend a value to a list, only if the list exists")
(defcmd :lists 1.0.0 :lrange (key start stop) :list "Get a range of elements from a list")
(defcmd :lists 1.0.0 :lrem (key count value) :integer "Remove elements from a list")
(defcmd :lists 1.0.0 :lset (key index value) :status "Set the value of an element in a list by its index")
(defcmd :lists 1.0.0 :ltrim (key start stop) :status "Trim a list to the specified range")
(defcmd :lists l.0.0 :rpop (key) :string "Remove and get the last element in a list")
(defcmd :lists 1.2.0 :rpoplpush (source destination) :string "Remove the last element in a list, append it to another list and return it")
(defcmd :lists 1.0.0 :rpush (key value . _) :integer "Append one or multiple values to a list")
(defcmd :lists 2.2.0 :rpushx (key value) :integer "Append a value to a list, only if the list exists")

;; sets
(defcmd :sets 1.0.0 :sadd (key member . _) :integer "Add one or more members to a set")
(defcmd :sets 1.0.0 :scard (key) :integer "Get the number of members in a set")
(defcmd :sets 1.0.0 :sdiff (key . _) :list "Subtract multiple sets")
(defcmd :sets 1.0.0 :sdiffstore (destination key . _) :integer "Subtract multiple sets and store the resulting set in a key")
(defcmd :sets 1.0.0 :sinter (key . _) :list "Intersect multiple sets")
(defcmd :sets 1.0.0 :sinterstore (destination key . _) :integer "Intersect multiple sets and store the resulting set in a key")
(defcmd :sets 1.0.0 :sismember (key member) :boolean "Determine if a given value is a member of a set")
(defcmd :sets 1.0.0 :smembers (key) :list "Get all members in a set")
(defcmd :sets 1.0.0 :smove (source destination member) :boolean "Move a member from one set to another")
(defcmd :sets 1.0.0 :spop (key) :string "Remove and return a random member from a set")
(defcmd :sets 1.0.0 :srandmember (key) :string "Get a random member from a set")
(defcmd :sets 1.0.0 :srem (key member . _) :integer "Remove one or more members from a set")
(defcmd :sets 1.0.0 :sunion (key . _) :list "Add multiple sets")
(defcmd :sets 1.0.0 :sunionstore (destination key . _) :integer "Add multiple sets and store the resulting set in a key")

;; sorted sets
(defcmd :sorted-sets 1.2.0 :zadd (key score member . _) :integer "Add one or more members to a sorted set, or update its score if it already exists")
(defcmd :sorted-sets 1.2.0 :zcard (key) :integer "Get the number of members in a sorted set")
(defcmd :sorted-sets 2.0.0 :zcount (key min max) :integer "Count the members in a sorted set with scores within the given values")
(defcmd :sorted-sets 1.2.0 :zincrby (key increment member) :number "Increment the score of a member in a sorted set")
(defcmd :sorted-sets 2.0.0 :zinterstore (destination numkeys key . _) :integer "Intersect multiple sorted sets and store the resulting sorted set in a new key")
(defcmd :sorted-sets 1.2.0 :zrange (key start stop . _) :list "Return a range of members in a sorted set, by index") ; TODO: できればwithscoreがついた時は、返す値の型を変えるようにしたい・・・
(defcmd :sorted-sets 1.0.5 :zrangebyscore (key min max . _) :list "Return a range of members in a sorted set, by score")
(defcmd :sorted-sets 2.0.0 :zrank (key member) :integer-or-null "Determine the index of a member in a sorted set")
(defcmd :sorted-sets 1.2.0 :zrem (key member . _) :integer "Remove one or more members from a sorted set")
(defcmd :sorted-sets 2.0.0 :zremrangebyrank (key start stop) :integer "Remove all members in a sorted set within the given indexes")
(defcmd :sorted-sets 1.2.0 :zremrangebyscore (key min max) :integer "Remove all members in a sorted set within the given scores")
(defcmd :sorted-sets 1.2.0 :zrevrange (key start stop . _) :list "Return a range of members in a sorted set, by index, with scores ordered from high to low")
(defcmd :sorted-sets 2.2.0 :zrevrangebyscore (key max min . _) :list "Return a range of members in a sorted set, by score, with scores orderes from high to low")
(defcmd :sorted-sets 2.0.0 :zrevrank (Key member) :integer-or-null "Determine the index of a member in a sorted set, with scores ordered from high to low")
(defcmd :sorted-sets 1.2.0 :zscore (key member) :number "Get the score associated with the given member in a sorted set")
(defcmd :sorted-sets 2.0.0 :zunionstore (destination numkeys key . _) :integer "Add multiple sorted sets and store the resulting sorted set in a new key")

;; pub/sub
;; TODO

;; transaction
(defcmd :transaction 2.0.0 :discard () :true "Discard all commands issued after MULTI")
(defcmd :transaction 1.2.0 :exec () :list "Execute all commands issued after multi")
(defcmd :transaction 1.2.0 :multi () :true "Mark the start of a transaction block")
(defcmd :transaction 2.2.0 :unwatch () :true "Forget about all watched keys")
(defcmd :transaction 2.2.0 :watch (key . _) :true "Watch the given keys to determine execution of the MULTI/EXEC block")

;; scripting
;; TODO

;; connection
(defcmd :connection 1.0.0 :auth (password) :status "Authenticate to the server")
(defcmd :connection 1.0.0 :echo (message) :string "Echo the given string")
(defcmd :connection 1.0.0 :ping () :status "Ping the server")
(defcmd :connection 1.0.0 :quit () :true "Close the connection")
(defcmd :connection 1.0.0 :select (index) :status "Change the selected database for the current connection")
