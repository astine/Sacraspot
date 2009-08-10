#! /usr/bin/sbcl --core ~/serv --script

(print *posix-argv*)

(use-package 'postmodern)
(use-package 'local-time)

(defmacro while (condition &body body)
  `(do () ((not ,condition)) ,@body))

(defun reload-settings (file-path)
  (with-open-file (settings file-path)
    (while (listen settings)
      (let ((setting (read settings)))
	(eval `(setq ,(first setting) ,(rest setting)))))))

;;load all of the settings
(reload-settings "sacraspot/settings.sexp")

;; database
(defvar *connection-spec* `(,*db-name* ,*db-user* ,*db-passwd* ,*db-host* :pooled-p t))

;(connect *db-name* *db-user* *db-passwd* *db-host* :pooled-p t)

(defvar *headway* 1)
(setf *default-timezone* +utc-zone+)

(defun add-events (date)
  (let* ((date-string (concatenate 'string (cl-postgres:to-sql-string date) "::date"))
	 (mon-to-char (concatenate 'string "TO_CHAR(" date-string ",E'Mon')"))
	 (dow-to-char (concatenate 'string "TO_CHAR(" date-string ",E'dy')")))
    (execute (:insert-into 'events
		(:select 'parish_id 'schedule_id (:as (:+ (:raw date-string) 'start-time) 'time)
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
  (execute (:delete-from 'events
			 :where (:< 'time (:type date :date)))))

(defun refresh-events (date)
  (execute (:delete-from 'events))
  (dotimes (offset (1+ *headway*))
    (add-events (timestamp+ date offset :day))))

(defun update-events (date)
  (add-events (timestamp+ date *headway* :day))	
  (clear-events date))

(with-connection *connection-spec*
  (cond ((equal (second *posix-argv*) "refresh")
	 (refresh-events (now)))
	((equal (second *posix-argv*) "update")
	 (update-events (now)))))

(with-open-file (log "/var/log/sacraspot" :direction :output :if-exists :append)
  (format log "~A:~T~A" "manage-events~%" (second *posix-argv*)))

(quit)
