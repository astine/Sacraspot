;;;sacraspot intialize.lisp - Andrew Stine (C) 2009

(cl:in-package #:sacraspot)

(defmacro while (condition &body body)
  `(do () ((not ,condition)) ,@body))

(defmacro load-settings (file-path) 
  (with-open-file (settings file-path)
    (let ((outform nil))
      (while (listen settings)
	(let ((setting (read settings)))
	  (push `(defvar ,(first setting) ,(rest setting)) outform)))
      `(progn ,@outform))))

(defun reload-settings (file-path)
  (with-open-file (settings file-path)
    (while (listen settings)
      (let ((setting (read settings)))
	(eval `(setq ,(first setting) ,(rest setting)))))))

;;load all of the settings
(load-settings "settings.sexp")

;; database

(defvar *connection-spec* `(,*db-name* ,*db-user* ,*db-passwd* ,*db-host* :pooled-p t))

;; server

(defvar *acceptor* (make-instance 'acceptor :port *port*))
;(start *acceptor*)

;logs
(setq *message-log-pathname* (merge-pathnames *home* "/logs/messages"))
(setq *access-log-pathname* (merge-pathnames *home* "/logs/access"))

;debugging
(if *debug* (setq *show-lisp-errors-p* t))

;; ajax callbacks

(defparameter *ajax-handler-uri* "/ajax")

(defparameter *ajax-processor* (make-ajax-processor 
				:type :prototype
				:server-uri *ajax-handler-uri*
				:js-file-uris (namestring 
					       (merge-pathnames *js-dir* "/prototype.js"))))

;(with-input-from-string (stream "9300 Stonewall Road
;Manassas, VA 20110-2594")
	     ;(let ((st-add nil)
		   ;(street nil))
	       ;(if (between (char-code (peek-char nil stream)) 47 58)
		   ;(progn (setq st-add (read stream))
			  ;(setq street (read-line stream))))
	       ;(list st-add street)))
