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

;; transaction
(defcmd :transaction 2.0.0 :discard () :true "Discard all commands issued after MULTI")
(defcmd :transaction 1.2.0 :exec () :list "Execute all commands issued after multi")
(defcmd :transaction 1.2.0 :multi () :true "Mark the start of a transaction block")
(defcmd :transaction 2.2.0 :unwatch () :true "Forget about all watched keys")
(defcmd :transaction 2.2.0 :watch (key . _) :true "Watch the given keys to determine execution of the MULTI/EXEC block")


