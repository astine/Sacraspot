;;;website - Andrew Stine (C) 2009-2010

(cl:in-package #:cl-user)

(defpackage #:sacraspot
  (:shadowing-import-from #:js #:! #:true #:false)
  (:use #:cl 	        ;Common Lisp
	#:cl-user       ;Lisp userland, included so I can work within the ss package
	#:cl-ppcre      ;Regular Expressions
	#:hunchentoot   ;Lisp webserver	
	#:parenscript   ;Javascript generation language
;	#:drakma        ;Lisp web client
	#:cl-who        ;HTML generation library
	#:local-time    ;For consistent date+time handling
	#:js 	
	#:postmodern    ;Database access layer
	#:yason         ;JSON parser/generator
	#:html-template ;html-templating engine
	)
  (:export #:alist-to-plist 
	   #:with-gensyms 
	   #:group 
	   #:aif 
	   #:awhile 
	   #:arc-if
	   #:get-range
	   #:range
	   #:make-set
	   #:call-with
	   #:fetch-parameter
	   #:parse-number-span
	   #:make-number-span
	   #:clean-phone
	   #:pretty-print-phone
	   #:format-hr-timestamp

	   #:with-cached
	   #:defcached

	   #:parse-csv-stream
	   #:parse-csv

	   #:normalize-ip
	   #:get-locale
	   #:latitude-and-longitude
	   
	   #:while
	   #:load-settings
	   #:reload-settings
	   
	   #:query-sacraments
	   #:query-sacraments*
	   #:select-parishes
	   #:select-parishes*
	   #:select-schedules
	   #:select-schedules*
	   #:insert-parishe
	   #:insert-parishes
	   #:insert-schedule
	   #:insert-schedules
	   
	   #:to-list
	   #:numbers-to-listitem
	   #:numbers-to-months
	   #:numbers-to-dows
	   ))


(defpackage #:sacraspot-tests ;contains sacraspot test-cases
  (:use #:cl
	#:cl-user
	#:sacraspot
	#:cl-ppcre
	#:fiveam))

(in-package #:sacraspot)


