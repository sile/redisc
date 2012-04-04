(in-package :redisc)

(defun connect (&key (host "localhost") (port 6379) timeout)
  (usocket:socket-connect host port 
                          :timeout timeout
                          :element-type 'octet
                          :nodelay :if-supported))

(defun close (connection)
  (usocket:socket-close connection))

#|
(redisc:q :set "a" 10)
(redisc:q* '(:set "a" 10) :connection *default-connection*)

(redisc:multi (:set "a" 10) (:get "b"))
(redisc:multi* '((:set "a" 10) (:get "a")))

;; この中ではmulti呼び出しは禁止する
(redisc:with-watch (a b c)  *default-connection* 
  (if (zerop (redisc:get "a"))
      :cancel ; -> これは不要にする? 空リストを返すようにすれば、unwatchするのと同様の動作を実現できる
    '((:set "a" 10) (:get "a"))))
|#
