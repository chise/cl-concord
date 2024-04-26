(defsystem :cl-concord
  :description "CONCORD implementation based on Common Lisp"
  :version "0.3"
  :author "Tomohiko Morioka"
  :licence "LGPL"
  :depends-on (:cl-redis)
  :serial t
  :components ((:file "cl-concord")))
