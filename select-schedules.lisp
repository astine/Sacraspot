;;; website select-parishes.lisp - Andrew Stine (C) 2009-2010

(in-package #:sacraspot)

;;query elements:
;parish-id
;sacrament-type
;start-time
;end-time
;details
;dom
;dow
;month
;year

(defun select-schedules (parish-id sacrament-type start-time end-time language details dom dow month year)
  "Selects a set of schedules from the db according to the given filters, and returns the results
   encoded in a json string."
  (let ((prev-row nil))
    (yason:with-output-to-string* ()
      (yason:with-array ()
	(doquery (:order-by
		  (:select 'schedule_id 'parish-id 'sacrament-type
			   'start-time 'end-time 'language 'details 'years 'months 'days_of_month 'days_of_week
			   :from 'schedules_full :where
			   (:in 'schedule_id
				(:select 'schedule_id :from 'full_schedules :where
					 (:raw (sql-compile
						(remove nil
						`(:and
						  t
						  ,(when parish-id `(:= parish_id ,parish-id))
						  ,(when sacrament-type (cons ':or (mapcar (lambda (s-t)
											   `(:= sacrament_type ,s-t))
											 sacrament-type)))
						  ,(when start-time `(:= start_time ,start-time))
						  ,(when end-time `(:= end_time ,end-time))
						  ,(when language `(:= language ,language))
						  ,(when details `(:= details ,details))
						  ,(when dom `(:= day_of_month ,dom))
						  ,(when dow `(:= day_of_week ,dow))
						  ,(when month `(:= month ,month))
						  ,(when year `(:= year ,year)))))))))
		  'schedule_id)
	    (schedule-id parish-id sacrament-type start-time end-time language details years months doms dows)
	  (yason:with-object ()
	    (yason:encode-object-element "SCHEDULE-ID" schedule-id)
	    (yason:encode-object-element "PARISH-ID" parish-id)
	    (yason:encode-object-element "SACRAMENT-TYPE" sacrament-type)
	    (yason:encode-object-element "START-TIME" (format-hr-timestamp start-time))
	    (yason:encode-object-element "END-TIME" (format-hr-timestamp end-time))
	    (yason:encode-object-element "LANGUAGE" language)
	    (yason:encode-object-element "DETAILS" details)
	    (yason:encode-object-element "YEARS" (coalesce years))
	    (yason:encode-object-element "MONTHS" (coalesce months))
	    (yason:encode-object-element "DOMS" (coalesce doms))
	    (yason:encode-object-element "DOWS" (coalesce dows))))))))

(define-easy-handler (select-schedules* :uri "/select-schedules" :default-request-type :post) ()
  "Handler for select-schedules passes the parameters to select-schedules"
  (with-connection *connection-spec*
    (select-schedules (fetch-parameter "parish-id")
		      (fetch-parameter "sacrament-type" nil
				       (lambda (item)
					 (mapcar #'string-capitalize
						 (split-sequence:split-sequence #\, item))))
		      (fetch-parameter "start-time")
		      (fetch-parameter "end-time")
		      (fetch-parameter "language")
		      (fetch-parameter "details")
		      (fetch-parameter "dom")
		      (fetch-parameter "dow" nil #'string-downcase)
		      (fetch-parameter "month" nil #'string-downcase)
		      (fetch-parameter "year"))))
