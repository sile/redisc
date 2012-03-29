(defpackage :redisc
  (:use :common-lisp)
  (:shadow :common-lisp close set get)
  (:export connect
           close
           
           *varsion* ;; version of redis
           *default-connection* 

           ;; command: key
           del exists expire expireat keys

           ;; command: string
           set get
           ))
(in-package :redisc)

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(vector octet))

(defparameter *delim* 
  (map 'octets #'char-code '(#\Return #\Newline)))
