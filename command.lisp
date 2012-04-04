(in-package :redisc)

(defparameter *default-connection* nil)

(defun name-to-command (name &aux (name (symbol-name name)))
  (let ((p (position #\- name)))
    (if (null p)
        (list name)
      (list (subseq name 0 p) (subseq name (1+ p))))))

;; TODO: redisのドキュメントを付与する
(defmacro defcmd (name arity result-type &key vary)
  (let ((args (loop REPEAT arity COLLECT (gensym))))
    ;; TODO: いろいろ整理
    `(defun ,name (,@args ,@(if vary '(&rest v) ()) &aux (connection *default-connection*)) 
       (multiple-value-bind (value ok) (request connection ,(car (name-to-command name))
                                                (cl:append ',(cdr (name-to-command name))
                                                       ,(if vary
                                                            `(nconc (list ,@args) v)
                                                          `(list ,@args))))
         (if (not ok)
             (values value ok)
           (values (convert ,result-type value) ok))))))

(defun convert (type value)
  (ecase type
    (:object value)
    (:string value) ; TODO: assert
    (:integer value) ; TODO: assert
    (:boolean (ecase value (1 t) (0 nil)))
    (:list value)
    (:status value)
    (:ok     (string= "OK" value)) ; TODO: assert
    ))

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

(defun symb (&rest args) ; XXX:
  (intern (format nil "~{~a~}" args)))

(eval-when (:compile-toplevel :load-toplevel)
(defun ensure-list (x)
  (if (atom x)
      (if (null x)
          x
        (list x))
    (cons (car x) (ensure-list (cdr x)))))
)

(defmacro defcmd2 (category version name args result-type description)
  (let ((connection (gensym))
        (vars (gensym))) ; XXX:
    `(progn 
       (push (make-cmd :name ,name
                       :version ,(symbol-name version)
                       :category ,category
                       :args ',args
                       :return-type ,result-type
                       :description ,description
                       :fn (lambda (,connection ,vars &key pipe)
                             (destructuring-bind ,args ,vars
                               (declare (ignore ,@(ensure-list args))))
                             (request ,connection ,name ,vars :pipe pipe))
                       )
             *command-list*))))

(defun get-cmd (name)
  (find name *command-list* :key #'cmd-name)) ; TODO: error-check

(defun q* (name args &key (connection *default-connection*))
  (funcall (cmd-fn (get-cmd name)) connection args))

(defun q (name &rest args)
  (q* name args))

(defun pipe (commands &key (connection *default-connection*))
  (loop FOR (name . args) IN commands
        DO (funcall (cmd-fn (get-cmd name)) connection args :pipe t))
  (let ((out (usocket:socket-stream connection)))
    (force-output out)
    (loop REPEAT (length commands)
          COLLECT (multiple-value-list (read-reply out))))) ; 多値の代わりにエラーオブジェクトを返しても良いかもしれない

(defcmd2 :strings 2.0.0 :append (key value) :integer "Append a value to a key")
(defcmd2 :strings 1.0.0 :decr (key) :integer "Decrement the integer value of a key by one")
(defcmd2 :strings 1.0.0 :decrby (key decrement) :integer "Decrement the integer value of a key by the given number")
(defcmd2 :strings 1.0.0 :get (key) :string "Get the value of a key")
(defcmd2 :strings 2.2.0 :getbit (key offset) :integer "Returns the bit value at offset in the string value stored at key")
(defcmd2 :strings 2.4.0 :getrange (key start end) :string "Get a substring of the string stored at a key")
(defcmd2 :strings 1.0.0 :getset (key value) :string "Set the string value of a key and return its old value")
(defcmd2 :strings 1.0.0 :incr (key) :integer "Increment the integer value of a key by one")
(defcmd2 :strings 1.0.0 :incrby (key increment) :integer "Increment the integer value of a key by the given amount")
(defcmd2 :strings 2.6.0 :incrbyfloat (key increment) :number "Increment the float value of a key by the given amount")
(defcmd2 :strings 1.0.0 :mget (key . _) :list "Get the values of all the given keys")
(defcmd2 :strings 1.0.1 :mset (key value . _) :true "Set multiple keys to multiple values")
(defcmd2 :strings 1.0.1 :msetnx (key value . _) :boolean "Set multiple keys to multiple values, only if none of the keys exist")
(defcmd2 :strings 2.6.0 :psetex (key milliseconds value) :true "Set the value and expiration in milliseconds of a key")
(defcmd2 :strings 1.0.0 :set (key value) :true "Set the string value of a key")
(defcmd2 :strings 2.2.0 :setbit (key offset value) :integer "Sets or clears the bit at offset in the string value stored at key")
(defcmd2 :strings 2.0.0 :setex (key seconds value) :true "Set the value and expiration of a key")
(defcmd2 :strings 2.2.0 :setrange (key offset value) :integer "Overwrite part of a string at key starting at the specified offset")
(defcmd2 :strings 2.2.0 :strlen (key) :integer "Get the length of the value stored in a key")

