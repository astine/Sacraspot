;;;sacraspot intialize.lisp - Andrew Stine (C) 2009-2010

(cl:in-package #:sacraspot)

(defmacro while (condition &body body)
  `(do () ((not ,condition)) ,@body))

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

;; ajax callbacks

; this stuff is for use with ht-ajax - probable that we don't really need it
(defparameter *ajax-handler-uri* "/ajax")

(defparameter *ajax-processor* (make-ajax-processor 
				:type :prototype
				:server-uri *ajax-handler-uri*
				:js-file-uris (namestring 
					       (merge-pathnames *js-dir* "/prototype.js"))))

;;timezone

(setf *default-timezone* +utc-zone+)
(set-local-time-cl-postgres-readers)

;(with-input-from-string (stream "9300 Stonewall Road
;Manassas, VA 20110-2594")
	     ;(let ((st-add nil)
		   ;(street nil))
	       ;(if (between (char-code (peek-char nil stream)) 47 58)
		   ;(progn (setq st-add (read stream))
			  ;(setq street (read-line stream))))
	       ;(list st-add street)))
