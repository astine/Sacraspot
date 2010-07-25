(require 'cl)

(defmacro aif (condition on-true &rest on-false)
  `(let ((it ,condition))
     (if it ,on-true ,@on-false)))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))

(defun chop (string)
  "Removes last character from string"
  (substring string 0 (1- (length string))))

(defmacro* for-rows ((&optional (start (point-min)) (end (point-max))) &rest body)
  "Executes the body once for each row in the buffer with the
   point at the beginning of each row"
  (let ((home (gensym)))
    `(let ((,home (point)))
       (goto-char ,start)
       ,@body
       (while (< (line-end-position 2) ,end)
	 (forward-line)
	 ,@body)
       (goto-char ,home))))

(defstruct error "Holds error information" row field message)

(defun print-error (error)
  "Prints a quick description of an error"
  (format "Row: %i , Field: %i - %s"
	   (1+ (or (error-row error) -1))
	   (1+ (or (error-field error) -1))
	   (error-message error)))

(defun print-errors (errors)
  "Prints a series of errors"
  (mapconcat #'print-error errors "\n"))

(defun message-errors (errors default)
  "Prints a series of errors"
  (if errors
      (message (print-errors errors))
    (message default)))

(defmacro defset (name accessor)
  `(defun ,name (error new-value)
     (setf (,accessor error) new-value)
     error))

(defset set-row error-row)
(defset set-field error-field)
(defset set-message error-message)

(defcustom field-seperator ?\,
  "")

(defcustom field-delimiter ?\"
  "")

(defface csv-error-face
  '((((class color) (background light)) (:foreground "Red" :underline t :bold nil))
    (((class color) (background dark)) (:foreground "Red" :underline t :bold nil))
    (t (:underline t)))
  "Marks errors in csv file"
  :group 'csv)

(defface csv-bad-line-face
  '((((class color) (background light)) (:foreground "Orange" :underline t :bold nil))
    (((class color) (background dark)) (:foreground "Orange" :underline t :bold nil))
    (t (:underline t)))
  "Marks parts of lines in csv files that have errors but aren't errors themselves"
  :group 'csv)

(defun breakup-sequence ()
  ""
  (format "%c%c%c" field-delimiter field-seperator field-delimiter))

(defun break-row-into-fields (row)
  "Breaks a row into its constituent fields with its quotes stripped off"
  (split-string  (substring row 1 (1- (length row))) (breakup-sequence)))

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
			 :help-echo (error-message error))))
  error)


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
    (if (aand (position field-delimiter field)
	      (not (equal ?\\ (elt field (1- it)))))
	(push (make-error :message (concat "Missing or misplaced quotation marks: " field)) errors))
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
	  (unless (and (equal first-char field-delimiter)
		       (equal last-char field-delimiter))
	    (push (make-error :message "Missing or misplaced quotation marks") errors))
	  (append errors (loop for count from 0 for field in fields for templ in template
			       nconc (mapcar (lambda (er)
					       (set-field er count))
					     (validate-csv-field field templ))))))))

(defun validate-csv-at-point (&optional print-message) ;;FIXME - currently bombs if there is an error on the last row
  "Validates csv row at point against *template*
   meant for interactive and incremental validation
   returns error"
  (interactive "p")
  (when print-message
    (remove-overlays (line-beginning-position) (line-end-position)))
  (unless (= (point) (point-max))
    (let ((errors (validate-csv-row (chop (thing-at-point 'line)) *template*))
	  (current-line (1- (count-lines 1 (1+ (point))))))
      (dolist (err errors)
	(set-row err current-line)
	(mark-error err))
      (when print-message
	(message-errors errors (format "No errors for row %i." current-line)))
      errors)))

(defun validate-csv-region (&optional start end print-message)
  "Validates a region in the buffer against the *template*"
  (interactive "r\np")
  (remove-overlays start end)
  (let ((errors (list nil)))
    (for-rows (start end) (nconc errors (validate-csv-at-point)))
    (when print-message
      (message-errors (rest errors) "No errors found in region."))
    (rest errors)))

(defun validate-csv-buffer (&optional print-message)
  "Validates entire buffer against *template*"
  (interactive "p")
  (let ((errors (validate-csv-region (point-min) (point-max))))
    (when print-message
      (message-errors errors "No errors found in buffer."))
    errors))

;;; Emacs mode for csv file editing

(defvar csv-validate-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c v") 'validate-csv-at-point)
    (define-key map (kbd "C-c V") 'validate-csv-buffer)
    map)
  "Keymap for `csv-validate-mode'")

(defvar csv-validate-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\" "$\"" st)
    st)
  "Syntax Table for `csv-validate-mode'")

(defun paired-delimiter-at-point (&optional point)
  (eq (car (syntax-after (or point (point)))) 8))
(defun not-after-quote (&optional point)
  (not (eq (car (syntax-after (1- (or point (point))))) 9)))

(defun search-paired-delimiter (limit)
  "Searching function for font-lock-keywords;
   Returns true is an unquoted paired delimiter is found
   and sets the match data `match-data' to its location."
  (catch 'return
    (dotimes (x limit)
      (if (and (paired-delimiter-at-point) (not-after-quote))
	  (progn (set-match-data (list (point-marker) 
				       (progn (forward-char) (point-marker))))
		 (throw 'return t))
	(forward-char)))))

(defvar csv-validate-font-lock-keywords
  '((search-paired-delimiter . font-lock-builtin-face))
  "Keyword highlighting specification for `csv-validate-mode'.")

;(defun csv-validate-font-lock-syntactic-face-function (state)
  ;(cond ((and (nth 3 state)
	      ;;(push (elt (thing-at-point 'char) 0) gfoo)
	      ;(not (nth 5 state)))
	 ;font-lock-comment-face)))
	;(t font-lock-comment-face)))

(define-derived-mode csv-validate-mode fundamental-mode "CSV Validate"
  "A major mode for validating csv files."
  :syntax-table csv-validate-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults)
       '(csv-validate-font-lock-keywords))
	 ;nil nil nil nil))
	 ;(font-lock-syntactic-face-function
	  ;. csv-validate-font-lock-syntactic-face-function)))
  (set-syntax-table csv-validate-mode-syntax-table)
  (use-local-map csv-validate-mode-map)
					;(set (make-local-variable 'indent-line-function) 'csv-validate-indent-line)
					;(set (make-local-variable 'imenu-generic-expression)
					;csv-validate-imenu-generic-expression)
					;(set (make-local-variable 'outline-regexp) csv-validate-outline-regexp)
					;...)
  )

(provide 'csv-validate-mode)
