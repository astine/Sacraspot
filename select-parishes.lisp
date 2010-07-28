;;; website select-parishes.lisp - Andrew Stine (C) 2009-2010

(in-package #:sacraspot)

;; Fields in table:
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
; Queries can be filtered by these fields

(defun select-parishes (parish-id fullname shortname
			country state city street street-number zip
			phone email website
			latitude longitude diocese)
  "Queries the database for parishes, returning a JSON list of hashes each representing a
   returned parish. Results can be filtered by specifying the value for a specific field;
   leave a field blank to allow for any value."
  (macrolet ((make-objects (&body vars)
	       `(yason:with-object ()
		  ,@(mapcar (lambda (var)
			      `(yason:encode-object-element (string (quote ,var)) ,var))
			    vars)))
	     (make-sqls (&body vars)
	       `(list 'and ,@(mapcar (lambda (var)
				       `(if ,var (list ':= ',var ,var) t))
				     vars))))
    (yason:with-output-to-string* ()
      (yason:with-array ()
	(doquery (:select 'parish_id 'fullname 'shortname 'country 'state
			  'city 'street 'street_number 'zip 'phone 'email
			  'website 'latitude 'longitude 'diocese
			  :from 'parishes :where
			  (:raw (sql-compile
				 `(:and
				   ,(make-sqls parish-id fullname shortname country
					       state city street zip email website
					       latitude longitude diocese)
				   ,(if street-number `(:= street_number ,street-number) t)
				   ,(if phone `(:= phone ,(clean-phone phone)) t)))))
	    (parish-id fullname shortname country state city street street-number zip phone email website latitude longitude diocese)
	  (when (standard-phone-number-p phone)
	    (setf phone (pretty-print-phone phone)))
	  (setf latitude (write-to-string latitude))
	  (setf longitude (write-to-string longitude))
	  (make-objects parish-id fullname shortname country state
			city street street-number zip phone email
			website latitude longitude diocese))))))

(define-easy-handler (select-parishes* :uri "/select-parishes" :default-request-type :post) ()
  "Dispatches requests to select-parishes; calles select-parishes directly using parameters passed by the client"
  (with-connection *connection-spec*
    (apply #'select-parishes 
	   (mapcar #'fetch-parameter '("parish-id" "fullname" "shortname" "country" "state"
				       "city" "street" "street-number" "zip" "phone" "email"
				       "website" "latitude" "longitude" "diocese")))))
