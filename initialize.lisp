;;;sacraspot intialize.lisp - Andrew Stine (C) 2009-2010

(cl:in-package #:sacraspot)

(defmacro load-settings (file-path) 
  "Opens a file with sexp ordered pairs and of the form:
   (SYMBOL . VALUE) and for each pair, creates a global
   variable of the name SYMBOL and valule VALUE.
   Does so at compile time."
  (with-open-file (settings file-path)
    (let ((outform nil))
      (while (listen settings)
	(let ((setting (read settings)))
	  (push `(defvar ,(first setting) ,(rest setting)) outform)))
      `(progn ,@outform))))

(defun reload-settings (file-path)
  "Opens a file with sexp ordered pairs and of the form:
   (SYMBOL . VALUE) and for each pair, creates a global
   variable of the name SYMBOL and valule VALUE.
   Does so at run time."
  (with-open-file (settings file-path)
    (while (listen settings)
      (let ((setting (read settings)))
	(eval `(setq ,(first setting) ,(rest setting)))))))

;;load all of the settings
(load-settings "/home/illuminati/sacraspot/settings.sexp")

;; database

(defvar *connection-spec* `(,*db-name* ,*db-user* ,*db-passwd* ,*db-host* :pooled-p t)
  "Database Conncection Spec, Contains information for connecting to the database")

;; server

(defvar *acceptor* (make-instance 'acceptor :port *port*)
  "Server object, represents the Hunchentoot Lisp webserver")
(defvar *test-acceptor* (make-instance 'acceptor :port 8079)
  "Secondary server object")
;(start *acceptor*)

;logs
(setq *message-log-pathname* (merge-pathnames *home* "/logs/messages")) ;general log
(setq *access-log-pathname* (merge-pathnames *home* "/logs/access"))    ;logs connection attempts

;debugging
(if *debug* (setq *show-lisp-errors-p* t))

;;timezone
(setf *default-timezone* +utc-zone+)
(set-local-time-cl-postgres-readers)

;;scheduler
(start-cron)

;;logging    ;;Possibly will add an extended logging engine, if needed
;(defcategory maintenance-job)
;(defcategory access)
;
;(defvar *log-file* (open "sacraspot.messages"
			 ;:direction :output
			 ;:if-exists :append
			 ;:if-does-not-exist :create))
;
;(start-sender 'primary-logger
	      ;(stream-sender :location *log-file*)
	      ;:category-spec '(or mainenance-job access)
	      ;:output-spec '(time category message context))

(defun shutdown ()
  (close *log-file*))
