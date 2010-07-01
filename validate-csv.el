(require 'cl)

(defmacro aif (condition on-true &rest on-false)
  `(let ((it ,condition))
     (if it ,on-true ,@on-false)))

(defmacro for-rows (&rest body)
  (let ((home (gensym)))
    `(let ((,home (point)))
       (goto-char 1)
       (while (zerop (forward-line))
	 ,@body)
       (goto-char ,home))))

(defstruct error "Holds error information" row field message)

(defun print-error (error)
  (princ (format "Row: %i , Field: %i - %s\n"
		 (1+ (or (error-row error) 0))
		 (1+ (or (error-field error) 0))
		 (error-message error))))

(defun mark-error ()
  (put-text-property (line-beginning-position) (line-end-position) 'face 'highlight))

(defmacro defset (name accessor)
  `(defun ,name (error new-value)
     (setf (,accessor error) new-value)
     error))

(defset set-row error-row)
(defset set-field error-field)
(defset set-message error-message)

(defun string-field-p (field)
  (unless (symbolp (car (read-from-string field)))
    "Not a string"))

(defun integer-field-p (field)
  (unless (integerp (car (read-from-string field)))
    "Not a integer"))

(defvar *template* (list #'string-field-p #'integer-field-p))

(defun validate-csv-field (field &optional template)
  (let ((errors nil))
    (aif (and (functionp template)
	      (funcall template field))
      (push (make-error :message it) errors))
					;34 is '"' and 92 is '\' in ascii
					;checking that '\' always preceeds '"' within a field
    (let ((pos (position 34 field)))
      (if (and pos
	       (equal 92 (elt field (1- pos))))
	  (push (make-error :message "Missing or misplaced quotation marks") errors)))
    errors))

(defun validate-csv-row (row &optional template)
  (let ((errors nil))
    (if (not (zerop (length row)))
	(let ((fields (split-string (substring row 1 (1- (length row))) "\",\""))
	      (first-char (string-to-char row))
	      (last-char (string-to-char (substring row (1- (length row))))))
	  (when (and template (not (= (list-length template) (list-length fields))))
	    (push (make-error :message "Wrong number of fields") errors))
					;The first and last characters must be '"'
	  (unless (and (equal first-char 34)
		       (equal last-char 34))
	    (push (make-error :message "Missing or misplaced quotation marks") errors))
	  (append errors (loop for count from 0 for field in fields for templ in template
			       nconc (mapcar (lambda (er)
					       (set-field er count))
					     (validate-csv-field field templ))))))))

(defun chop (string)
  (substring string 0 (1- (length string))))

(defun validate-csv-at-point () ;;FIXME - currently bombs if there is an error on the last row
  (interactive)
  (unless (= (point) (point-max))
    (mapcar (lambda (err)
	      (print-error (set-row err (1- (count-lines 1 (1+ (point))))))
	      (mark-error))
	    (validate-csv-row (chop (thing-at-point 'line)) *template*))))

(defun validate-csv-buffer ()
  (interactive)
  (put-text-property (point-min) (point-max) 'face 'none)
  (for-rows (validate-csv-at-point)))



