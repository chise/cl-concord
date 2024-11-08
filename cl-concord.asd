(defsystem :cl-concord
  :description "CONCORD implementation based on Common Lisp"
  :version "1.0"
  :author "Tomohiko Morioka"
  :licence "LGPL"
  :depends-on (:cl-redis :cl-json)
  :serial t
  :components ((:file "cl-concord")
	       (:file "concord-json")))
