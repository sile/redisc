(defpackage :redisc
  (:use :common-lisp)
  (:shadow :common-lisp close apropos apropos-list describe)
  (:export connect close
           with-connection
           q q* q!
           receive-message
           apropos
           apropos-list
           describe
           *default-connection*))
(in-package :redisc)

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(vector octet))

(defparameter *delim* 
  (map 'octets #'char-code '(#\Return #\Newline)))
