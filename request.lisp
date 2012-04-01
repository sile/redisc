(in-package :redisc)

(defun args-to-octets (&rest args)
  (apply #'concatenate 
   'octets
   (loop FOR a IN args
         COLLECT (etypecase a
                   (list    (apply #'args-to-octets a))
                   (symbol  (string-to-octets (symbol-name a)))
                   (integer (string-to-octets (princ-to-string a)))
                   (string  (string-to-octets a))
                   (octets  a)))))

(defun build-one-arg (arg &aux (octets (args-to-octets arg)))
  (list "$" (length octets) *delim* octets *delim*))

(defun build-request (command args)
  (args-to-octets
   "*" (1+ (length args)) *delim*
   (build-one-arg command)
   (loop FOR a IN args APPEND (build-one-arg a))))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defconstant +REPLY_ONELINE+ (char-code #\+))
  (defconstant +REPLY_ERROR+   (char-code #\-))
  (defconstant +REPLY_NUMBER+  (char-code #\:))
  (defconstant +REPLY_BULK+    (char-code #\$))
  (defconstant +REPLY_MULTI+   (char-code #\*)))

;; TODO: socketとセットで保持するようにする(構造体)
(defparameter *read-buffur*
  (make-array 1024 :element-type 'octet))

(defparameter *r-delim* (reverse *delim*))
(defun read-oneline (in) ; XXX: 複雑すぎる
  (labels ((recur (acc tail)
             (if (not (mismatch *r-delim* tail))
                 (octets-to-string (coerce (nreverse acc) 'octets))
               (recur (cons (car (last tail)) acc)
                      (cons (read-byte in) (butlast tail))))))
    (recur '() (reverse (list (read-byte in) (read-byte in))))))

(defun read-string (buf in &key (length (length buf)))
  (read-sequence buf in :end length)
  (assert (loop FOR i FROM 0 BELOW (length *delim*)
                ALWAYS (= (read-byte in) (aref *delim* i))))
  (octets-to-string buf))

(defun read-bulk (in)
  (let ((buf *read-buffur*)
        (length (parse-integer (read-oneline in))))
    (cond ((= length -1)           nil)
          ((< (length buf) length) (read-string buf in :length length))
          (t                       (read-string (make-array length :element-type 'octet) in)))))

(defun read-multi (in)
  (let ((count (parse-integer (read-oneline in))))
    (if (= count -1)
        nil
      (loop REPEAT count COLLECT (read-reply in)))))

(defun read-reply (in)
  (ecase (read-byte in)
    (#.+REPLY_ONELINE+ (values (read-oneline in) t))
    (#.+REPLY_ERROR+   (values (read-oneline in) nil))
    (#.+REPLY_NUMBER+  (values (parse-integer (read-oneline in)) t))
    (#.+REPLY_BULK+    (values (read-bulk in) t))
    (#.+REPLY_MULTI+   (values (read-multi in) t))))

(defun request (connection command args &aux (out (usocket:socket-stream connection)))
  (write-sequence (build-request command args) out)
  (force-output out)
  
  (read-reply out))

(defun pipeline-request (connection &rest requests &aux (out (usocket:socket-stream connection)))
  (loop FOR (command . args) IN requests
        DO (write-sequence (build-request command args) out))
  (force-output out)
  (loop REPEAT (length requests)
        COLLECT (multiple-value-list (read-reply out))))
