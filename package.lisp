(defpackage :redisc
  (:use :common-lisp)
  (:shadow :common-lisp close)
  (:export connect
           close))
(in-package :redisc)

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(vector octet))

(defparameter *delim* 
  (map 'octets #'char-code '(#\Return #\Newline)))
