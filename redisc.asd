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
               (:file "request")
               (:file "command")
               (:file "command-keys")
               (:file "command-strings")
               (:file "command-hashes")
               (:file "command-lists")
               (:file "command-sets")
               (:file "command-sortedsets")
               (:file "command-pubsub")
               (:file "command-transactions")
               (:file "command-scripting")
               (:file "command-connection")
               (:file "command-server")
               (:file "redisc")))
