(in-package :asdf)

(defsystem redisc
  :name "redisc"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "A redis client"
  
  :serial t
  :depends-on (:usocket)
  :components ((:file "package")
               (:file "util")
               (:file "redisc")))
