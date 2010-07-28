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
			   'start-time 'end-time 'language 'details 'day_of_month 'day_of_week 'month 'year
			   :from 'full_schedules :where
			   (:in 'schedule_id
				(:select 'schedule_id :from 'full_schedules :where
					 (:raw (sql-compile
						`(:and
						  ,(if parish-id `(:= parish_id ,parish-id) t)
						  ,(if sacrament-type (cons ':or (mapcar (lambda (s-t)
											   `(:= sacrament_type ,s-t))
											 sacrament-type))
						       t)
						  ,(if start-time `(:= start_time ,start-time) t)
						  ,(if end-time `(:= end_time ,end-time) t)
						  ,(if language `(:= language ,language) t)
						  ,(if details `(:= details ,details) t)
						  ,(if dom `(:= day_of_month ,dom) t)
						  ,(if dow `(:= day_of_week ,dow) t)
						  ,(if month `(:= month ,month) t)
						  ,(if year `(:= year ,year) t) ))))))
		  'schedule_id)
	    (schedule-id parish-id sacrament-type start-time end-time language details dom dow month year)
	  (if (equal schedule-id (first prev-row))
	      (progn (if (coalesce dom) (pushnew dom (eighth prev-row) :test #'equal))
		     (if (coalesce dow) (pushnew dow (ninth prev-row) :test #'equal))
		     (if (coalesce month) (pushnew month (tenth prev-row) :test #'equal))
		     (if (coalesce year) (pushnew year (nth 10 prev-row) :test #'equal)))
	      (progn (if prev-row
			 (yason:with-object ()
			   (yason:encode-object-element "PARISH-ID" (second prev-row))
			   (yason:encode-object-element "SACRAMENT-TYPE" (third prev-row))
			   (yason:encode-object-element "START-TIME" (format-hr-timestamp (fourth prev-row)))
			   (yason:encode-object-element "END-TIME" (format-hr-timestamp (fifth prev-row)))
			   (yason:encode-object-element "LANGUAGE" (sixth prev-row))
			   (yason:encode-object-element "DETAILS" (seventh prev-row))
			   (yason:encode-object-element "DOM" (eighth prev-row))
			   (yason:encode-object-element "DOW" (ninth prev-row))
			   (yason:encode-object-element "MONTH" (tenth prev-row))
			   (yason:encode-object-element "YEAR" (nth 11 prev-row))))
		     (setf prev-row 
			   (mapcar #'coalesce
				   (list schedule-id parish-id sacrament-type start-time end-time language details
					 (to-list dom) (to-list dow) (to-list month) (to-list year)))))))))))

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
