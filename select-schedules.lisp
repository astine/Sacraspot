;;; website select-parishes.lisp - Andrew Stine (C) 2009

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

(defun to-list (item)
  (and (coalesce item) (list item)))

(defun select-schedules (parish-id sacrament-type start-time end-time details dom dow month year)
  (let ((prev-row nil))
    (yason:with-output-to-string* ()
      (yason:with-array ()
	(doquery (:order-by
		  (:select 'schedule_id 'parish-id 'sacrament-type
			   'start-time 'end-time 'details 'dom 'dow 'month 'year
			   :from 'full_schedules :where 
			   (:raw (sql-compile
				  `(:or
				    ,(if parish-id `(:= parish_id ,parish-id))
				    ,(if sacrament-type `(:= sacrament_type ,sacrament-type))
				    ,(if start-time `(:= start_time ,start-time))
				    ,(if end-time `(:= end_time ,end-time))
				    ,(if details `(:= details ,details))
				    ,(if dom `(:= dom ,dom))
				    ,(if dow `(:= dow ,dow))
				    ,(if month `(:= month ,month))
				    ,(if year `(:= year ,year))))))
		  'schedule_id)
	(schedule-id parish-id sacrament-type start-time end-time details dom dow month year)
	  (if (= schedule-id (first prev-row))
	      (progn (pushnew dom (seventh prev-row))
		     (pushnew dow (eighth prev-row))
		     (pushnew dow (ninth prev-row))
		     (pushnew dow (tenth prev-row)))
	      (progn (yason:with-object ()
		       (yason:encode-object-element "parish-id" (second prev-row))
		       (yason:encode-object-element "sacrament-type" (third prev-row))
		       (yason:encode-object-element "start-time" (format-hr-timestamp (fourth prev-row)))
		       (yason:encode-object-element "end-time" (format-hr-timestamp (fifth prev-row)))
		       (yason:encode-object-element "details" (sixth prev-row))
		       (yason:encode-object-element "dom" (seventh prev-row))
		       (yason:encode-object-element "dow" (eighth prev-row))
		       (yason:encode-object-element "month" (ninth prev-row))
		       (yason:encode-object-element "year" (tenth prev-row)))
		     (setf prev-row 
			   (list (schedule-id parish-id sacrament-type start-time end-time details
					      (to-list dom) (to-list dow) (to-list month) (to-list year))))
			   

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
	      (unless (or (null (coalesce details)) (equal details ""))
		  (htm (:tr (:td "Details") (:td (str details)))))
	      (:tr (:td
		    (:form :id "delete-schedule" :action "admin" :method "post"
			   (:input :type "hidden" :name "action" :value "delete-schedule")
			   (:input :type "hidden" :name "schedule" :value schedule-id)
			   (:input :type "submit" :value "Delete Schedule"))))))))))

(define-easy-handler (select-schedules* :uri "/select-schedules" :default-request-type :post) ()
  (with-connection *connection-spec*
    (apply #'select-parishes 
	   (mapcar #'fetch-parameter '("parish-id" "sacrament-type" "start-time" "end-time" "details"
				       "dom" "dow" "month" "year")))))
