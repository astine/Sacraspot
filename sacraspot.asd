;;; website sacraspot.asd - Andrew Stine (c) 2009

(asdf:defsystem :sacraspot
  :description "sacraspot"
  :version "0.0.1"
  :serial t
  :components ((:file "packages")
               (:file "initialize")
               (:file "admin"))
  :depends-on (:hunchentoot
	       :drakma
               :parenscript
	       :trivial-shell
	       :local-time
	       :cl-who
               :cl-ppcre
	       :fiveam
	       :ht-ajax
	       :postmodern
	       :yason))
