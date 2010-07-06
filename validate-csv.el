(require 'cl)

(defmacro aif (condition on-true &rest on-false)
  `(let ((it ,condition))
     (if it ,on-true ,@on-false)))

(defun chop (string)
  "Removes last character from string"
  (substring string 0 (1- (length string))))

(defmacro for-rows (&rest body)
  "Executes the body once for each row in the buffer with the
   point at the beginning of each row"
  (let ((home (gensym)))
    `(let ((,home (point)))
       (goto-char 1)
       (while (zerop (forward-line))
	 ,@body)
       (goto-char ,home))))

(defstruct error "Holds error information" row field message)

(defun print-error (error)
  "Prints a quick description of an error"
  (princ (format "Row: %i , Field: %i - %s\n"
		 (1+ (or (error-row error) -1))
		 (1+ (or (error-field error) -1))
		 (error-message error))))

(defmacro defset (name accessor)
  `(defun ,name (error new-value)
     (setf (,accessor error) new-value)
     error))

(defset set-row error-row)
(defset set-field error-field)
(defset set-message error-message)

(defface csv-error-face
  '((((class color) (background light)) (:foreground "Red" :underline t :bold nil))
    (((class color) (background dark)) (:foreground "Red" :underline t :bold nil))
    (t (:underline t)))
  "Marks errors in csv file"
  :group 'csv)

(defface csv-bad-line-face
  '((((class color) (background light)) (:forground "Orange" :underline t :bold nil))
    (((class color) (background dark)) (:forground "Orange" :underline t :bold nil))
    (t (:underline t)))
  "Marks parts of lines in csv files that have errors but aren't errors themselves"
  :group 'csv)

(defun break-row-into-fields (row)
  "Breaks a row into its constituent fields with its quotes stripped off"
  (split-string  (substring row 1 (1- (length row))) "\",\""))

(defun get-field-limits (field &optional row)
  "Returns the begin and end position in text of a field in its particular row"
  (let ((fields (break-row-into-fields (or row (thing-at-point 'line)))))
    (when (>= field (length fields))
      (error "field index %i out of bounds for row: %s" field row))
    (let* ((start (apply #'+ (cons (* field 3) (mapcar #'length (butlast fields (- (length fields) field))))))
	   (end (+ start 2 (length (nth field fields)))))
      (list start end))))

(defun create-overlay (start end &rest properties)
  "Convenience function to make an overlay with some properties already set"
  (let ((overlay (make-overlay start end)))
    (loop for (key value) on properties by #'cddr
	  do (overlay-put overlay (intern (substring (symbol-name key) 1)) value))
    overlay))

(defun mark-error (error)
  "Marks current line as having error(s) specified by 'error'
   uses overlays to highlight the affected fields"
  (cond ((error-field error)
	 (destructuring-bind (start end) (get-field-limits (error-field error))
	   (let ((line-beginning (line-beginning-position)))
	     (create-overlay line-beginning (line-end-position)
			     :face 'csv-bad-line-face)
	     (create-overlay (+ line-beginning start) (+ line-beginning end)
			     :face 'csv-error-face
			     :priority 1
			     :help-echo (error-message error)))))
	(t
	 (create-overlay (line-beginning-position) (line-end-position)
			 :face 'csv-error-face
			 :priority 1
			 :help-echo (error-message error)))))


(defun string-field-p (field)
  (unless (symbolp (car (read-from-string field)))
    "Not a string"))

(defun integer-field-p (field)
  (unless (integerp (car (read-from-string field)))
    "Not a integer"))

(defcustom *template* (list #'string-field-p #'integer-field-p)
  "Template against which a buffer is validated;
   This is a list of functions which return nil on success or an error
   message if there is something wrong with a field")

(defun validate-csv-field (field &optional template)
  "Validates text in 'field', making sure that it has no stray quotes and satisfies template"
  (let ((errors nil))
    (aif (and (functionp template)
	      (funcall template field))
      (push (make-error :message it) errors))
					;checking that '\' always preceeds '"' within a field
    (let ((pos (position ?\" field)))
      (if (and pos
	       (equal ?\\ (elt field (1- pos))))
	  (push (make-error :message "Missing or misplaced quotation marks") errors)))
    errors))

(defun validate-csv-row (row &optional template)
  "Validates row against template, breaking row into fields and checking each field with 
   validate-csv-field"
  (let ((errors nil))
    (if (not (zerop (length row)))
	(let ((fields (break-row-into-fields row))
	      (first-char (string-to-char row))
	      (last-char (string-to-char (substring row (1- (length row))))))
	  (when (and template (not (= (list-length template) (list-length fields))))
	    (push (make-error :message "Wrong number of fields") errors))
					;The first and last characters must be '"'
	  (unless (and (equal first-char ?\")
		       (equal last-char ?\"))
	    (push (make-error :message "Missing or misplaced quotation marks") errors))
	  (append errors (loop for count from 0 for field in fields for templ in template
			       nconc (mapcar (lambda (er)
					       (set-field er count))
					     (validate-csv-field field templ))))))))

(defun validate-csv-at-point () ;;FIXME - currently bombs if there is an error on the last row
  "Validates csv row at point against *template*
   meant for interactive and incremental validation"
  (interactive)
  (unless (= (point) (point-max))
    (mapcar (lambda (err)
	      (print-error (set-row err (1- (count-lines 1 (1+ (point))))))
	      (mark-error err))
	    (validate-csv-row (chop (thing-at-point 'line)) *template*))))

(defun validate-csv-buffer ()
  "Validates entire buffer against *template*"
  (interactive)
  (remove-overlays)
  (for-rows (validate-csv-at-point)))



