(in-package :redisc)

(defun connect (&key (host "localhost") (port 6379) timeout)
  ;; TODO: finalizerを設定しておいた方が良いっぽい
  (usocket:socket-connect host port 
                          :timeout timeout
                          :element-type 'octet
                          :nodelay :if-supported))

(defun close (connection)
  (usocket:socket-close connection))

(defmacro with-connect ((&optional (connection-var '*default-connection*)) 
                        (&key (host "localhost") (port 6379) timeout)
                        &body body)
  `(let ((,connection-var (connect :host ,host :port ,port :timeout ,timeout)))
     (unwind-protect
         (locally ,@body)
       (close ,connection-var))))

(defmacro with-default ((connection) &body body)
  `(let ((*default-connection* ,connection))
     ,@body))

