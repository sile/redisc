(defpackage :redisc
  (:use :common-lisp)
  (:shadow :common-lisp close apropos apropos-list describe)
  (:export *default-connection*
           connect close
           with-connection
           q q* cmd
           receive
           apropos
           apropos-list
           describe))
(in-package :redisc)

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(vector octet))

(defparameter *delim* 
  (map 'octets #'char-code '(#\Return #\Newline)))
