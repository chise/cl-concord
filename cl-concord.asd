(defsystem :cl-concord
  :description "CONCORD implementation based on Common Lisp"
  :version "1.2"
  :author "Tomohiko Morioka"
  :licence "LGPL"
  :depends-on (:trivial-utf-8
	       :cl-redis :cl-json :cl-ipfs-api2)
  :serial t
  :components ((:file "cl-concord")
	       (:file "chise-support")
	       (:file "concord-json")
	       (:file "concord-ipld")))
