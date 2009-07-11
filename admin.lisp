;;; website admin.lisp - Andrew Stine (C) 2009

(in-package #:sacraspot)

(defun alist-to-plist (alist)
  (mapcan #'(lambda (pair) 
	      (list (car pair) (cdr pair)))
	  alist))

;(defun parse-mailing-address (address)
  ;((with-input-from-string (address-stream address))))

;(defun parse-mailing-address (address)
  ;(all-matches ".*([0-9][0-9][0-9][0-9][0-9]-?[0-9]?[0-9]?[0-9]?[0-9]?)$" address))

(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar #'(lambda (symbol)
		     `(,symbol (gensym)))
		 symbols)
     ,@body))

(defmacro defcached (name params &body body)
  (with-gensyms (table value found)
    `(let ((,table (make-hash-table :test #'equal)))
       (defun ,name ,params
	 (multiple-value-bind (,value ,found)
	     (gethash (list ,@params) ,table)
	   (if ,found
	       ,value
	       (setf (gethash (list ,@params) ,table)
		     (progn ,@body))))))))

(defun group (list n)
  (cond ((null list)
	 nil)
	((<= (list-length list) n)
	 (list list))
	(t
	 (cons (subseq list 0 n)
	       (group (subseq list n) n)))))
  
(defmacro aif (condition &body body)
  `(let ((it ,condition))
     (if it ,@body)))

(defmacro awhile (condition &body body)
  `(do ((it ,condition ,condition)) 
       ((not it)) ,@body))

(defmacro arc-if (&body forms)
  `(cond ,@(mapcar #'(lambda (clause)
		       (if (= (list-length clause) 2)
			   clause
			   (cons 't clause)))
		   (group forms 2))))

(defun get-range (begin end)
  (if (<= begin end)
      (cons begin (get-range (1+ begin) end))))

(defmacro range (begin &optional end)
  (arc-if (and (listp begin)
	       (null end))
	  `(apply #'get-range ,begin)
	  (and (integerp begin)
	       (integerp end))
	  `(list ,@(get-range begin end))
	  `(funcall #'get-range ,begin ,end)))

(defun make-set (list &optional (eql-test #'=) (predicate #'<))
  (labels ((filter-dups (lst)
	     (arc-if (null lst)
		     nil
		     (equal (list-length lst) 1)
		     lst
		     (funcall eql-test (first lst) (second lst))
		     (filter-dups (rest lst))
		     (cons (first lst) (filter-dups (rest lst))))))
    (filter-dups (sort list predicate))))

(defun parse-number-span (span)
  (make-set
   (mapcan #'(lambda (part)
	       (arc-if (cl-ppcre:scan "[0-9]+-[0-9]+" part)
		       (get-range (mapcar #'read-from-string
				      (split-sequence:split-sequence #\- part)))
		       (cl-ppcre:scan "[0-9]+" part)
		       (list (read-from-string part))))
	   (split-sequence:split-sequence #\, span :remove-empty-subseqs t))))

(defun clean-phone (number)
  (with-output-to-string (out)
    (with-input-from-string (in number)
      (awhile (read-char in nil nil)
	(if (and (> (char-code it) 47) 
		 (< (char-code it) 58))
	    (princ it out))))))
					   
(defun pretty-print-phone (number)
  (concatenate 'string 
	       "(" 
	       (subseq number 0 3) 
	       ") " 
	       (subseq number 3 6) 
	       "-" 
	       (subseq number 6)))

(defvar states '(("AL" . "Alabama") ("AK" . "Alaska") ("AZ" . "Arizona") ("AR" . "Arkansas") 
		 ("CA" . "California") ("CO" . "Colorado") ("CT" . "Connecticut") ("DE" . "Delaware") 
		 ("DC" . "District Of Columbia") ("FL" . "Florida") ("GA" . "Georgia") ("HI" . "Hawaii") 
		 ("ID" . "Idaho") ("IL" . "Illinois") ("IN" . "Indiana") ("IA" . "Iowa") 
		 ("KS" . "Kansas") ("KY" . "Kentucky") ("LA" . "Louisiana") ("ME" . "Maine")
		 ("MD" . "Maryland") ("MA" . "Massachusetts") ("MI" . "Michigan") ("MN" . "Minnesota")
		 ("MS" . "Mississippi") ("MO" . "Missouri") ("MT" . "Montana") ("NE" . "Nebraska") 
		 ("NV" . "Nevada") ("NH" . "New Hampshire") ("NJ" . "New Jersey") ("NM" . "New Mexico") 
		 ("NY" . "New York") ("NC" . "North Carolina") ("ND" . "North Dakota") ("OH" . "Ohio") 
		 ("OK" . "Oklahoma") ("OR" . "Oregon") ("PA" . "Pennsylvania") ("RI" . "Rhode Island")
		 ("SC" . "South Carolina") ("SD" . "South Dakota") ("TN" . "Tennessee") ("TX" . "Texas")
		 ("UT" . "Utah") ("VT" . "Vermont") ("VA" . "Virginia") ("WA" . "Washington")
		 ("WV" . "West Virginia") ("WI" . "Wisconsin") ("WY" . "Wyoming")))

(defvar months '(("Jan" . "January")("Feb" . "February")("Mar" . "March")
		 ("Apr" . "April")("May" . "May") ("Jun" . "June")
		 ("Jul" . "July")("Aug" . "August") ("Sep" . "September")
		 ("Oct" . "October")("Nov" . "November") ("Dec" . "December")))

(defvar dows '(("Mon" . "Monday") ("Tue" . "Tuesday") ("Wed" . "Wednesday") ("Thu" . "Thursday")
	       ("Fri" . "Friday") ("Sat" . "Saturday") ("Sun" . "Sunday")))
		 

(define-easy-handler (admin :uri "/admin" :default-request-type :post) ()
  (with-connection *connection-spec*
    (if (parameter "action")            ;if request came from admin page form
					;dispatch on action type
	(cond ((equal (parameter "action") "add-parish") 
	       (query (:insert-into 'parishes :set 
				    'fullname (parameter "parish-long-name")
				    'shortname (parameter "parish-short-name")
				    'country (parameter "parish-country")
				    'state (parameter "parish-state")
				    'county (parameter "parish-county")
				    'city (parameter "parish-city")
				    'street (parameter "parish-street")
				    'street_number (parameter "parish-street-number")
				    'zip (parameter "parish-zip")
				    'phone (clean-phone (parameter "parish-phone"))
				    'email (parameter "parish-email")
				    'website (parameter "parish-website")
				    'diocese (parameter "parish-diocese"))))
	      ((equal (parameter "action") "delete-parish")
	       (execute (:delete-from 'parishes :where (:= 'parish_id (parameter "parish")))))
	      ;((equal (parameter "action") "add-schedule")
	       ;((execute (:insert-into 'schedules :set
				       ;'parish_id (parameter "parish")
				       ;'sacrament_type (parameter "sacrament-type")
				       ;'start_time (parameter "start-time")
				       ;'end_time (parameter "end-time")))
		;(execute (:insert-into 'schedule_year_map :set
				       )
    (let ((parishes (query (:select '* :from 'parishes) :alists)))
      (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
	(:html
	 (:head 
	  (:title "admin"))
	 (:body
	  (:div :style "float:left"
	   (:form :id "add-parish" :action "admin" :method "post" :style "float:left"
	    (:input :type "hidden" :name "action" :value "add-parish")
	    (:input :type "text" :name "parish-long-name" "parish-long-name")(:br)
	    (:input :type "text" :name "parish-short-name" "parish-short-name")(:br)
	    (:select :name "parish-country" (:option :value "US" "United States")) (:br)
	    (:select :name "parish-state" 
		     (mapcar #'(lambda (state) 
				 (htm (:option :value (car state) 
					       (str (cdr state)))))
			     states)) (:br)
	    (:input :type "text" :name "parish-county" "parish-county")(:br)
	    (:input :type "text" :name "parish-city" "parish-city")(:br)
	    (:input :type "text" :name "parish-street" "parish-street")(:br)
	    (:input :type "text" :name "parish-street-number" "parish-street-number")(:br)
	    (:input :type "text" :name "parish-zip" "parish-zip")(:br)
	    (:input :type "text" :name "parish-phone" "parish-phone")(:br)
	    (:input :type "text" :name "parish-email" "parish-email")(:br)
	    (:input :type "text" :name "parish-website" "parish-website")(:br)
	    (:input :type "text" :name "parish-diocese" "parish-diocese")(:br)
	    (:input :type "submit" :value "submit"))
	   (:form :id "add-schedule" :action "admin" :method "post" :style "float:left"
            (:input :type "hidden" :name "action" :value "add-schedule")
	    (:input :type "text" :name "parish" "Parish")(:br)
	    (:select :name "sacrament-type" "Sacrament Kind"
		     (:option :value "mass" "Mass")
		     (:option :value "confession" "Confession"))(:br)
            (:input :type "text" :name "start-time" "Start Time")(:br)
	    (:input :type "text" :name "end-time" "End Time")(:br)
	    (:input :type "text" :name "years" "Years")(:br)
	    (:fieldset (:legend "Months")
	     (mapcar #'(lambda (month)
		         (htm (:input :type "checkbox"
				      :name (car month)
				      (str (cdr month)))(:br)))
		     months))(:br)
            (:input :type "text" :name "days" "Days of the Month")(:br)
	    (:fieldset (:legend "Days of the Week")
	     (mapcar #'(lambda (dow)
		         (htm (:input :type "checkbox"
				      :name (car dow)
				      (str (cdr dow)))(:br)))
		     dows))(:br)
	    (:input :type "submit" :value "submit")))
	  (:div :style "clear:left"
	  (mapcar #'(lambda (parish)
		      (htm (:div :style "float:left;"
			    (:table
			     (fmt "~{<tr><td>~A</td><td>~A</td></tr>~}" 
				  (alist-to-plist 
				   (mapcar #'(lambda (pair)
					       (if (and (equal (symbol-name (first pair))
							       "PHONE")
							(not (equal (cdr pair) "")))
						   (cons (car pair) (pretty-print-phone (cdr pair)))
						   pair))
					   (rest parish))))) 
			   (:br)
			   (:form :id "delete-parish" :action "admin" :method "post"
			    (:input :type "hidden" :name "action" :value "delete-parish")
			    (:input :type "hidden" :name "parish" :value (cdar parish)
			    (:input :type "submit" :value "delete")))
			   (:br))))
		  parishes))))))))


(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
  (:form :id "add-schedule" :action "admin" :method "post"
   (:input :type "hidden" :name "action" :value "add-schedule")
   (:input :type "text" :name "parish" "Parish")(:br)
   (:select :name "sacrament-type" "Sacrament Kind"
	    (:option :value "mass" "Mass")
	    (:option :value "confession" "Confession"))(:br)
   (:input :type "text" :name "start-time" "Start Time")(:br)
   (:input :type "text" :name "end-time" "End Time")(:br)
   (:input :type "text" :name "years" "Years")(:br)
   (mapcar #'(lambda (month)
	       (htm (:input :type "checkbox"
			    :name (cdr month))))
	   months)"Months"(:br)
   (:input :type "text" :name "days" "Days of the Month")(:br)
   (mapcar #'(lambda (dow)
	       (htm (:input :type "checkbox"
			    :name (cdr dow))))
	   dows)"Days of the Week"(:br)))

