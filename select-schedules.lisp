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

(defun select-schedules (fullname shortname
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

(defun parish-schedule-html (parish-id &optional style)
  "Generates html to display out a parishes schedules"
  (with-html-output-to-string (*standard-output* nil :indent t :prologue nil)
    (:table :style style
     (dolist (schedule (query (:select 'schedule_id 'sacrament_type 'start_time 'end_time 'details
				       :from 'schedules
				       :where (:= 'parish_id parish-id))))
       (destructuring-bind (schedule-id sacrament-type start-time end-time details) 
	   schedule
	 (htm (:tr (:td "Schedule") (:td (str schedule-id)))
	      (:tr (:td "Type") (:td (str sacrament-type)))
	      (:tr (:td "Time") (:td (str (format nil "~A - ~A" 
						  (format-time-of-day start-time)
						  (format-time-of-day end-time)))))
	      (aif (query (:select 'year :from 'schedule_year_map
				   :where (:= 'schedule_id schedule-id)) :column)
		(htm (:tr (:td "Years") (:td (str (reduce #'(lambda (x y)
							      (concatenate 'string x ", " y))
							  it))))))
	      (aif (query (:select 'month :from 'schedule_month_map
				   :where (:= 'schedule_id schedule-id)) :column)
		(htm (:tr (:td "Months") (:td (str (reduce #'(lambda (x y)
							       (concatenate 'string x ", " y))
							   it))))))
	      (aif (query (:select 'day_of_month :from 'schedule_dom_map
				   :where (:= 'schedule_id schedule-id)) :column)
		(htm (:tr (:td "Days of the Month") (:td (str (make-number-span it))))))
	      (aif (query (:select 'day_of_week :from 'schedule_dow_map
				   :where (:= 'schedule_id schedule-id)) :column)
		(htm (:tr (:td "Days of the Week") (:td (str (reduce #'(lambda (x y)
									 (concatenate 'string x ", " y))
								     it))))))
	      (unless (or (null details) (equal details :NULL) (equal details ""))
		  (htm (:tr (:td "Details") (:td (str details)))))
	      (:tr (:td
		    (:form :id "delete-schedule" :action "admin" :method "post"
			   (:input :type "hidden" :name "action" :value "delete-schedule")
			   (:input :type "hidden" :name "schedule" :value schedule-id)
			   (:input :type "submit" :value "Delete Schedule"))))))))))

(define-easy-handler (select-schedules* :uri "/select-schedules" :default-request-type :post) ()
  (with-connection *connection-spec*
    (apply #'select-parishes 
	   (mapcar #'fetch-parameter '("fullname" "shortname" "country" "state" "city"
				       "street" "street-number" "zip" "phone" "email"
				       "website" "latitude" "longitude" "diocese")))))
