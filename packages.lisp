;;;website - Andrew Stine (C) 2009-2010

(cl:in-package #:cl-user)

(defpackage #:sacraspot
  ;(:shadowing-import-from #:js #:!)
  (:shadowing-import-from #:cl-who #:escape-string #:*escape-char-p* #:escape-string-minimal #:escape-string-minimal-plus-quotes #:escape-string-all)
  (:shadowing-import-from #:cl #:+ #:- #:++ #:>= #:<= #:< #:> #:= #:cons #:throw #:lambda #:null)
  (:shadowing-import-from #:log5 #:log-message)
  (:use #:cl 	        ;Common Lisp
	#:cl-user       ;Lisp userland, included so I can work within the ss package
	#:cl-ppcre      ;Regular Expressions
	#:hunchentoot   ;Lisp webserver	
	;#:js            ;Javascript generation language
;	#:drakma        ;Lisp web client
	#:ironclad      ;crypto library
	#:cl-who        ;HTML generation library
	#:local-time    ;For consistent date+time handling
	#:postmodern    ;Database access layer
	#:yason         ;JSON parser/generator
	#:html-template ;html-templating engine
	#:cl-cron       ;scheduler
	#:log5          ;logging utility
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
	   
	   #:query-parishes
	   #:query-parishes*
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
	   
	   #:*headway*
	   #:*update-or-refresh*
	   #:add-events
	   #:clear-events
	   #:refresh-events
	   #:update-events
	   #:manage-events
	   ))


(defpackage #:sacraspot-tests ;contains sacraspot test-cases
  (:use #:cl
	#:cl-user
	#:sacraspot
	#:cl-ppcre
	#:fiveam))

(in-package #:sacraspot)


