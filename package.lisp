(defpackage :redisc
  (:use :common-lisp)
  (:shadow :common-lisp close)
  (:export connect
           close))
(in-package :redisc)
