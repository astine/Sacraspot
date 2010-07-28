;; website insert-schedules.lisp - Andrew Stine (C) 2009

;;This file contains the web interface for inserting schedules
;;it accepts a single http parameters 'schedules' which should
;;contain csv data
;;
;;the csv fields are as follows:
;;
;;parish:         The parish numerical id, generated as the primary 
;;                  key for each parish
;;sacrament-type: Text identifier for either "Mass" or "Confession"
;;start-time:     Time of day the event begins
;;end-time:       Time of day the event ends
;;details:        A text field containing information about this
;;                  not present in the other fields
;;years:          All the year for which the event is valid; a numberspan
;;months:         All the months for which the event is valid; a numberspan
;;doms:           All the days of the month for the event;  a numberspan
;;dows:           All the days of the week for the event; a numberspan

(in-package #:sacraspot)

(defvar months '(("Jan" . "January")("Feb" . "February")("Mar" . "March")
		 ("Apr" . "April")("May" . "May") ("Jun" . "June")
		 ("Jul" . "July")("Aug" . "August") ("Sep" . "September")
		 ("Oct" . "October")("Nov" . "November") ("Dec" . "December")))

(defvar dows '(("Mon" . "Monday") ("Tue" . "Tuesday") ("Wed" . "Wednesday") ("Thu" . "Thursday")
	       ("Fri" . "Friday") ("Sat" . "Saturday") ("Sun" . "Sunday")))

(defun numbers-to-listitem (numbers list &optional (itemtype "item"))
  (unless (< (list-length numbers) (list-length list))
    (error (format nil "~A list is out of range or malformed: ~A" itemtype list)))
  (mapcar (lambda (number)
	    (unless (and (> number 0) (< number (list-length list)))
	      (error (format nil "~A is out of range: ~A" itemtype number)))
	    (rest (nth (1- number) list)))
	  numbers))

(defun numbers-to-months (numbers)
  (numbers-to-listitem numbers months "month"))

(defun numbers-to-dows (numbers)
  (numbers-to-listitem numbers dows "day-of-week"))

(defun insert-schedule (parish sacrament-type start-time end-time details
			years months doms dows)
  "Adds a schedule entry to the 'schedules table as well as entries to the 
   subordinate year, month, day-of-month, and day of week tables"
  ;if date filters are passed in as number spans, parse them to sets first
  (when (stringp years) (setf years (parse-number-span years)))
  (when (stringp months) (setf months (parse-number-span months)))
  (when (stringp doms) (setf doms (parse-number-span doms)))
  (when (stringp dows) (setf dows (parse-number-span dows)))
  (with-transaction ()
    (execute (:insert-into 'schedules :set
			   'parish_id parish
			   'sacrament_type (string-capitalize sacrament-type)
			   'start_time start-time
			   'end_time end-time
			   'details details))
    (dolist (year years)
      (execute (:insert-into 'schedule_year_map :set 'year year)))
    (dolist (month (numbers-to-months months))
      (execute (:insert-into 'schedule_month_map :set 'month (string-capitalize month))))
    (dolist (dom doms)
      (execute (:insert-into 'schedule_dom_map :set 'day_of_month dom)))
    (dolist (dow (numbers-to-dows dows))
      (execute (:insert-into 'schedule_dow_map :set 'day_of_week (string-downcase dow))))))


(define-easy-handler (insert-schedules :uri "/insert-schedules" :default-request-type :post) ()
  (let ((schedules (fetch-parameter "schedules" nil #'parse-csv)))
    (with-connection *connection-spec*
      (dolist (schedule schedules)
	(apply #'insert-schedule schedule)))))
      

