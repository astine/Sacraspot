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
			   'start-time 'end-time 'details 'day_of_month 'day_of_week 'month 'year
			   :from 'full_schedules :where 
			   (:raw (sql-compile
				  `(:and
				    ,(if parish-id `(:= parish_id ,parish-id) t)
				    ,(if sacrament-type `(:= sacrament_type ,sacrament-type) t)
				    ,(if start-time `(:= start_time ,start-time) t)
				    ,(if end-time `(:= end_time ,end-time) t)
				    ,(if details `(:= details ,details) t)
				    ,(if dom `(:= day_of_month ,dom) t)
				    ,(if dow `(:= day_of_week ,dow) t)
				    ,(if month `(:= month ,month) t)
				    ,(if year `(:= year ,year) t)
				    ))))
		  'schedule_id)
	    (schedule-id parish-id sacrament-type start-time end-time details dom dow month year)
	  (if (equal schedule-id (first prev-row))
	      (progn (if (coalesce dom) (pushnew dom (seventh prev-row)))
		     (if (coalesce dow) (pushnew dow (eighth prev-row))
		     (if (coalesce month) (pushnew month (ninth prev-row))
		     (if (coalesce year) (pushnew year (tenth prev-row)))
	      (progn (if prev-row
			 (yason:with-object ()
			   (yason:encode-object-element "parish-id" (second prev-row))
			   (yason:encode-object-element "sacrament-type" (third prev-row))
			   (yason:encode-object-element "start-time" (format-hr-timestamp (fourth prev-row)))
			   (yason:encode-object-element "end-time" (format-hr-timestamp (fifth prev-row)))
			   (yason:encode-object-element "details" (sixth prev-row))
			   (yason:encode-object-element "dom" (seventh prev-row))
			   (yason:encode-object-element "dow" (eighth prev-row))
			   (yason:encode-object-element "month" (ninth prev-row))
			   (yason:encode-object-element "year" (tenth prev-row))))
		     (setf prev-row 
			   (mapcar #'coalesce
				   (list schedule-id parish-id sacrament-type start-time end-time details
					 (to-list dom) (to-list dow) (to-list month) (to-list year)))))))))))

(define-easy-handler (select-schedules* :uri "/select-schedules" :default-request-type :post) ()
  (with-connection *connection-spec*
    (apply #'select-parishes 
	   (mapcar #'fetch-parameter '("parish-id" "sacrament-type" "start-time" "end-time" "details"
				       "dom" "dow" "month" "year")))))
