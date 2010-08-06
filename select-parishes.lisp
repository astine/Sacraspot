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

(defun generate-parishes-query (parish-id fullname shortname
				country state city street street-number zip
				phone email website
				latitude longitude diocese)
  (declare (type (or string null)
		 fullname shortname country state city street phone email website diocese)
	   (type (or integer null) parish-id street-number zip)
	   (type (or float integer null) latitude longitude))
  "Generates a query for selecting parishes from the database"
  (sql (:select 'parish_id 'fullname 'shortname 'country 'state
		'city 'street 'street_number 'zip 'phone 'email
		'website 'latitude 'longitude 'diocese
		:from 'parishes 
		:where (:raw (sql-compile
			      (remove nil
				      `(:and
					t
					,(when parish-id `(:= parish_id ,parish-id))
					,(when fullname `(:= fullname ,fullname))
					,(when shortname `(:= shortname ,shortname))
					,(when country `(:= country ,country))
					,(when state `(:= state ,state))
					,(when city `(:= city ,city))
					,(when street `(:= street ,street))
					,(when street-number `(:= street_number ,street-number))
					,(when zip `(:= zip ,zip))
					,(when phone `(:= phone ,phone))
					,(when email `(:= email ,email))
					,(when website `(:= website ,website))
					,(when latitude `(:= latitude ,latitude))
					,(when longitude `(:= longitude ,longitude))
					,(when diocese `(:= diocese ,diocese)))))))))

(defun select-parishes (parish-id fullname shortname
			country state city street street-number zip
			phone email website
			latitude longitude diocese)
  "Queries the database for parishes, returning a JSON list of hashes each representing a
   returned parish. Results can be filtered by specifying the value for a specific field;
   leave a field blank to allow for any value."
  (declare (type (or string null)
		 fullname shortname country state city street phone email website diocese)
	   (type (or integer null) parish-id street-number zip)
	   (type (or float integer null) latitude longitude))
  (macrolet ((make-objects (&body vars)
	       `(yason:with-object ()
		  ,@(mapcar (lambda (var)
			      `(yason:encode-object-element (string (quote ,var)) ,var))
			    vars))))
    (yason:with-output-to-string* ()
      (yason:with-array ()
	(doquery (:raw (generate-parishes-query parish-id fullname shortname
						country state city street street-number zip
						(when phone (clean-phone phone)) email website
						latitude longitude diocese))
	    (parish-id fullname shortname country state city street street-number zip phone email website latitude longitude diocese)
	  (when (standard-phone-number-p phone)
	    (setf phone (pretty-print-phone phone)))
	  (setf latitude (write-to-string latitude))
	  (setf longitude (write-to-string longitude))
	  (make-objects parish-id fullname shortname country state
			city street street-number zip phone email
			website latitude longitude diocese))))))

(defun find-parish-id (fullname shortname country state city street street-number 
		       zip phone email website latitude longitude diocese)
  "Returns the ID of the parish identified by the given fields, errs if more than one
   parish is returned"
  (declare (type (or string null)
		 fullname shortname country state city street phone email website diocese)
	   (type (or integer null) street-number zip)
	   (type (or float integer null) latitude longitude))
  (let ((results (query (:raw (generate-parishes-query nil fullname shortname country state city street street-number
						       zip phone email website latitude longitude diocese)))))
    (unless (= (list-length results) 1)
      (error "Returned too many results ~A" results))
    (first (first results))))

(define-easy-handler (select-parishes* :uri "/select-parishes" :default-request-type :post) ()
  "Dispatches requests to select-parishes; calles select-parishes directly using parameters passed by the client"
  (with-connection *connection-spec*
    (select-parishes (fetch-parameter "parish-id" :typespec '(or integer null))
		     (fetch-parameter "fullname" :typespec '(or string null))
		     (fetch-parameter "shortname" :typespec '(or string null))
		     (fetch-parameter "country" :typespec '(or string null))
		     (fetch-parameter "state" :typespec '(or string null))
		     (fetch-parameter "city" :typespec '(or string null))
		     (fetch-parameter "street" :typespec '(or string null))
		     (fetch-parameter "street-number" :typespec '(or integer null))
		     (fetch-parameter "zip" :typespec '(or integer null))
		     (fetch-parameter "phone" :typespec '(or string null))
		     (fetch-parameter "email" :typespec '(or string null))
		     (fetch-parameter "website" :typespec '(or string null))
		     (fetch-parameter "latitude" :typespec '(or float null))
		     (fetch-parameter "longitude" :typespec '(or float null))
		     (fetch-parameter "diocese" :typespec '(or string null)))))
