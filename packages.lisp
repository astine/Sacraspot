;;;website - Andrew Stine (C) 2009

(cl:in-package #:cl-user)

(defpackage #:sacraspot
  (:shadowing-import-from #:js #:! #:true #:false)
  (:use #:cl 	      ;Common Lisp
	#:cl-user     ;Lisp userland, included so I can work within the ss package
	#:cl-ppcre    ;Regular Expressions
	#:hunchentoot ;Lisp webserver	
	#:parenscript ;Javascript generation language
;	#:drakma      ;Lisp web client
	#:cl-who      ;HTML generation library
	#:local-time  ;For consistent date+time handling
	#:js 	
	#:ht-ajax     ;Hunchentoot ajax toolkit (should be removed)
	#:postmodern  ;Database access layer
	#:yason       ;JSON parser/generator
	)
  (:export #:admin))

(in-package #:sacraspot)
