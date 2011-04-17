;;; website manage-events.lisp - Andrew Stine (C) 2009-2010

(in-package #:sacraspot)

(defvar *headway* 1
  "The number of days into the future to prepare events to query against")

(defun add-events (date)
  "Populates the events table with a list of events for 'date' based on the schedules table."
  (let* ((date-string (concatenate 'string (cl-postgres:to-sql-string date) "::date"))
	 (mon-to-char (concatenate 'string "TO_CHAR(" date-string ",E'Mon')"))
	 (dow-to-char (concatenate 'string "TO_CHAR(" date-string ",E'dy')")))
    (execute (:insert-into 'events
		(:select 'parish_id 'schedule_id (:as (:+ (:raw date-string) 'start-time) 'time) :distinct
			 :from 'full_schedules
			 :where (:and (:or (:= 'year (:extract 'year (:raw date-string)))
					   (:is-null 'year))
				      (:or (:= (:type 'month :text) (:raw mon-to-char))
					   (:is-null 'month))
				      (:or (:= 'day_of_month (:extract 'day (:raw date-string)))
					   (:is-null 'day_of_month))
				      (:or (:= (:type 'day_of_week :text) (:raw dow-to-char))
					   (:is-null 'day_of_week))
				      (:not (:exists (:select '* :from 'events
							:where (:= (:+ (:raw date-string) 'start-time) 'time))))))))))

(defun clear-events (date)
  "Empties the events table"
  (execute (:delete-from 'events
			 :where (:< 'time (:type date :date)))))

(defun refresh-events (date)
  "Completely repopulates the events table from scratch"
  (execute (:delete-from 'events))
  (dotimes (offset (1+ *headway*))
    (add-events (timestamp+ date offset :day))))

(defun update-events (date)
  "Removes old events from the events table and adds new ones"
  (add-events (timestamp+ date *headway* :day))	
  (clear-events date))

(defvar *update-or-refresh* :update
  "Specifies whether the repeating task to manage events fully refreshed the events table or merely updates it")

(defun manage-events ()
  "The repeating scheduler job for the events table"
  (with-connection *connection-spec*
    (case *update-or-refresh*
      (:refresh (refresh-events (now)))
      (:update (update-events (now)))))
  (log-message 'info "~A called to manage events." *update-or-refresh*))

(add-cron-job 'manage-events :minute 0 :hour 0)
