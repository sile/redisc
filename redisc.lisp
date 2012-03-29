(in-package :redisc)

(defun connect (host &key (port 6379) timeout)
  (usocket:socket-connect host port :timeout timeout :nodelay :if-supported))

(defun close (connection)
  (usocket:socket-close connection))

