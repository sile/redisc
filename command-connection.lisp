(in-package :redisc)

(defcmd auth 1 :status)
(defcmd echo 1 :string)
(defcmd ping 0 :status)
(defcmd quit 0 :ok)
(defcmd select 1 :status)
