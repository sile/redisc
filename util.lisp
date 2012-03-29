(in-package :redisc)

(declaim (inline string-to-octets octets-to-string))

(defun string-to-octets (str)
  (sb-ext:string-to-octets str))

(defun octets-to-string (octets)
  (sb-ext:octets-to-string octets))
