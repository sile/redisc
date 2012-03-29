(in-package :redisc)

(defparameter *default-connection* nil)

;; TODO: redisのドキュメントを付与する
(defmacro defcmd (name arity result-type &key vary)
  (let ((args (loop REPEAT arity COLLECT (gensym))))
    `(defun ,name (,@args ,@(if vary '(&rest v) ()) &aux (connection *default-connection*))
       (multiple-value-bind (value ok) (request connection ,(symbol-name name) 
                                                ,(if vary
                                                     `(append (list ,@args) v)
                                                   `(list ,@args)))
         (if (not ok)
             (values value ok)
           (values (convert ,result-type value) ok))))))

(defun convert (type value)
  (ecase type
    (:object value)
    (:string value) ; TODO: assert
    (:integer value) ; TODO: assert
    (:boolean (ecase value (1 t) (0 nil)))
    (:ok     (string= "OK" value)) ; TODO: assert
    ))

;; key
(defcmd del 1 :integer :vary t)
(defcmd exists 1 :boolean)

;; string
(defcmd set 2 :ok)
(defcmd get 1 :string)

