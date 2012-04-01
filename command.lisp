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


