(in-package #:sacraspot)

(defun generate-query-parishes-query (parish-id latitude longitude distance maxresults)
  " Generates a query for selecting parishes by location and distance."
  (declare (type float latitude longitude) (type integer distance maxresults))
  (sql (:limit
	(:select 'parish_id 'fullname 'shortname 'country 'state
		 'city 'street 'street_number 'zip 'phone 'email
		 'website 'latitude 'longitude 'diocese
		 (:as (:ll_distance 'latitude latitude
				       'longitude longitude)
			 'distance)
		 :from 'parishes
		 :where (:and (:< (:ll_distance 'latitude latitude 
					       'longitude longitude)
				 distance)
			      (:raw (sql-compile (if parish-id `(:= parish_id ,parish-id) t)))))
	maxresults)))

(defun query-parishes (parish-id latitude longitude distance maxresults)
  "Queries for parishes by location and distance."
  (declare (type float latitude longitude) (type integer distance maxresults))
  (macrolet ((make-objects (&body vars)
	       `(yason:with-object ()
		  ,@(mapcar (lambda (var)
			      `(yason:encode-object-element (string (quote ,var)) ,var))
			    vars))))
    (yason:with-output-to-string* ()
      (yason:with-array ()
	(doquery (:raw (generate-query-parishes-query parish-id latitude longitude distance maxresults))
	    (parish-id fullname shortname country state city street street-number zip phone email website latitude longitude diocese distance)
	  (when (standard-phone-number-p phone)
	    (setf phone (pretty-print-phone phone)))
	  (setf latitude (write-to-string latitude))
	  (setf longitude (write-to-string longitude))
	  (make-objects parish-id fullname shortname country state
			city street street-number zip phone email
			website latitude longitude diocese
			distance))))))


(define-easy-handler (query-parishes* :uri "/query-parishes" :default-request-type :post) ()
  "Dispatches requests to query-parishes; calles query-parishes directly using parameters passed by the client"
  (with-connection *connection-spec*
    (with-location
      (query-parishes (fetch-parameter "parish_id" :typespec '(or integer null))
		      latitude longitude 
		      (fetch-parameter "distance" :typespec 'integer :default 25)
		      (fetch-parameter "maxresults" :typespec 'integer :default 25)))))
