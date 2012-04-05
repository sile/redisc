(defpackage :redisc
  (:use :common-lisp)
  (:shadow :common-lisp close ;set get sort type
                        ;append time 
           apropos apropos-list
           )
  (:export connect close
           with-connect with-default ;; => with-connection
           q q* q@ q!
           multi multi* multi@ 
           pipe pipe* pipe@
           with-watch 
           listen-message

           apropos
           apropos-list
           desc

           *default-connection*

           *varsion* ;; version of redis client => *version*
           )
  )
(in-package :redisc)

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(vector octet))

(defparameter *delim* 
  (map 'octets #'char-code '(#\Return #\Newline)))

(deftype exec-mode () '(member :single :pipe :multi :watch))

;; TODO: 別のファイルに移動
(defstruct connection
  (socket    t :type usocket:stream-usocket)
  (exec-mode t :type exec-mode))

;; TODO: 作成日時やドキュメントのバージョンを残しておく