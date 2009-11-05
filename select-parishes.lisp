;;; website select-parishes.lisp - Andrew Stine (C) 2009

(in-package #:sacraspot)

;;query elements:
;fullname
;shortname
;country
;state
;street
;street number
;zip code
;phone number
;email
;website
;latitude
;longitude
;diocese

(defun select-parishes (fullname shortname
			country state city street street-number zip
			phone email website
			latitude longitude diocese)
  (macrolet ((make-objects (&body vars)
	       `(yason:with-object ()
		  ,@(mapcar (lambda (var)
			      `(yason:encode-object-element (string (quote ,var)) ,var))
			    vars)))
	     (make-sqls (&body vars)
	       `(list 'or ,@(mapcar (lambda (var)
				      `(if ,var (list ':= ',var ,var)))
				    vars))))
    (yason:with-output-to-string* ()
      (yason:with-array ()
	(doquery (:select 'fullname 'shortname 'country 'state 'city
			  'street 'street_number 'zip 'phone 'email
			  'website 'latitude 'longitude 'diocese
			  :from 'parishes :where
			  (:raw (sql-compile
				 `(:or
				   ,(make-sqls fullname shortname country state
					       city street zip email website
					       latitude longitude diocese)
				   ,(if street-number `(:= street_number ,street-number))
				   ,(if phone `(:= phone ,(clean-phone phone)))))))
	    (fullname shortname country state city street street-number zip phone email website latitude longitude diocese)
	  (setf phone (pretty-print-phone phone))
	  (setf latitude (write-to-string latitude))
	  (setf longitude (write-to-string longitude))
	  (make-objects fullname shortname country state city
			street street-number zip phone email
			website latitude longitude diocese))))))

(define-easy-handler (select-parishes* :uri "/select-parishes" :default-request-type :post) ()
  (macrolet ((with-parameters ((&rest vars) &body body)
	       `(let ,(mapcar (lambda (var)
				`(,var (fetch-parameter (string ',var))))
			      vars)
		  ,@body)))
    (with-connection *connection-spec*
      (with-parameters (fullname shortname country state city street
				 street-number zip email website
				 latitude longitude diocese)
	(select-parishes fullname shortname country state city
			 street street-number zip phone email
			 website latitude longitude diocese)))))

