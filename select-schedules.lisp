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

(defun generate-schedules-query (parish-id sacrament-type start-time end-time language details dom dow month year)
  (sql (:order-by
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
						,(when dom `(:= days_of_month ,(coerce dom 'vector)))
						,(when dow `(:= days_of_week (:type ,(coerce dow 'vector) (array day_of_week))))
						,(when month `(:= months (:type ,(coerce month 'vector) (array month))))
						,(when year `(:= years ,(coerce year 'vector))))))))))
	'schedule_id)))

(defun select-schedules (parish-id sacrament-type start-time end-time language details dom dow month year)
  "Selects a set of schedules from the db according to the given filters, and returns the results
   encoded in a json string."
  (yason:with-output-to-string* ()
    (yason:with-array ()
      (doquery (:raw (generate-schedules-query parish-id sacrament-type start-time end-time language details dom dow month year))
	  (schedule-id parish-id sacrament-type start-time end-time language details years months doms dows)
	(yason:with-object ()
	  (yason:encode-object-element "SCHEDULE-ID" schedule-id)
	  (yason:encode-object-element "PARISH-ID" parish-id)
	  (yason:encode-object-element "SACRAMENT-TYPE" sacrament-type)
	  (yason:encode-object-element "START-TIME" (format-hr-timestamp (coalesce start-time)))
	  (yason:encode-object-element "END-TIME" (format-hr-timestamp (coalesce end-time)))
	  (yason:encode-object-element "LANGUAGE" language)
	  (yason:encode-object-element "DETAILS" details)
	  (yason:encode-object-element "YEARS" (coalesce years))
	  (yason:encode-object-element "MONTHS" (coalesce months))
	  (yason:encode-object-element "DOMS" (coalesce doms))
	  (yason:encode-object-element "DOWS" (coalesce dows)))))))

(defun find-schedule-id (parish-id sacrament-type start-time end-time
			 language details dom dow month year)
  "Returns the ID of the schedule identified by the given fields, errs if more than one
   schedule is returned"
  (let ((results (query (:raw (generate-schedules-query parish-id sacrament-type start-time end-time
							language details dom dow month year)))))
    (unless (= (list-length results) 1)
      (error "Returned too many results ~A" results))
    (first (first results))))

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
