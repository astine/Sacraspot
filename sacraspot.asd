;;; website sacraspot.asd - Andrew Stine (c) 2009

(asdf:defsystem :sacraspot
  :description "sacraspot"
  :version "0.1.1"
  :serial t
  :components ((:file "packages")
	       (:file "utilities")
	       (:file "caching")
	       (:file "csv")
	       (:file "ip-geolocation")
               (:file "initialize")
	       (:file "query-parishes")
	       (:file "query-sacraments")
	       (:file "select-parishes")
	       (:file "select-schedules")
	       (:file "insert-parishes")
	       (:file "insert-schedules")
	       (:file "tests")
	       (:file "frontpage")
               (:file "admin")
               (:file "bulk"))
  :depends-on (:asdf-system-connections
	       :hunchentoot
	       :drakma
               :parenscript
	       :trivial-shell
	       :local-time
	       :cl-who
	       :html-template
               :cl-ppcre
	       :fiveam
	       :postmodern
	       :yason))
