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

(defun generate-schedules-query (schedule-id parish-id sacrament-type start-time end-time language details years months doms dows)
  (declare (type (or integer null) schedule-id parish-id)
	   (type (or list null) sacrament-type years months doms dows)
	   (type (or local-time:timestamp null) start-time end-time)
	   (type (or string null) language details))
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
						,(when schedule-id `(:= schedule_id ,schedule-id))
						,(when parish-id `(:= parish_id ,parish-id))
						,(when sacrament-type (cons ':or (mapcar (lambda (s-t)
											   `(:= sacrament_type ,s-t))
											 sacrament-type)))
						,(when start-time `(:= start_time ,start-time))
						,(when end-time `(:= end_time ,end-time))
						,(when language `(:= language ,language))
						,(when details `(:= details ,details))
						,(when doms `(:= days_of_month ,(coerce doms 'vector)))
						,(when dows `(:= days_of_week (:type ,(coerce dows 'vector) (array day_of_week))))
						,(when months `(:= months (:type ,(coerce months 'vector) (array month))))
						,(when years `(:= years ,(coerce years 'vector))))))))))
	'schedule_id)))

(defun select-schedules (schedule-id parish-id sacrament-type start-time end-time language details years months doms dows)
  "Selects a set of schedules from the db according to the given filters, and returns the results
   encoded in a json string."
  (declare (type (or integer null) schedule-id parish-id)
	   (type (or list null) sacrament-type years months doms dows)
	   (type (or local-time:timestamp null) start-time end-time)
	   (type (or string null) language details))
  (yason:with-output-to-string* ()
    (yason:with-array ()
      (doquery (:raw (generate-schedules-query schedule-id parish-id sacrament-type start-time end-time language details years months doms dows))
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
			 language details years months doms dows)
  (declare (type (or integer null) parish-id)
	   (type (or list null) sacrament-type years months doms dows)
	   (type (or local-time:timestamp null) start-time end-time)
	   (type (or string null) language details))
  "Returns the ID of the schedule identified by the given fields, errs if more than one
   schedule is returned"
  (let ((results (query (:raw (generate-schedules-query nil parish-id (list sacrament-type)
							start-time end-time
							language details years months doms dows)))))
    (unless (= (list-length results) 1)
      (error "Returned too many results ~A" results))
    (first (first results))))

(define-easy-handler (select-schedules* :uri "/select-schedules" :default-request-type :post) ()
  "Handler for select-schedules passes the parameters to select-schedules"
  (with-connection *connection-spec*
    (select-schedules (fetch-parameter "schedule-id")
		      (fetch-parameter "parish-id")
		      (mapcar #'string-capitalize
			      (fetch-parameter "sacrament-type"))
		      (fetch-parameter "start-time")
		      (fetch-parameter "end-time")
		      (fetch-parameter "language")
		      (fetch-parameter "details")
		      (fetch-parameter "years")
		      (mapcar #'string-downcase
			      (fetch-parameter "months"))
		      (fetch-parameter "doms")
		      (mapcar #'string-downcase
			      (fetch-parameter "dows"))))) 
