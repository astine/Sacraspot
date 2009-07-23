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
		   (sacraspot::group forms 2))))

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
		       (range (mapcar #'read-from-string
				      (split-sequence:split-sequence #\- part)))
		       (cl-ppcre:scan "[0-9]+" part)
		       (list (read-from-string part))))
	   (split-sequence:split-sequence #\, span :remove-empty-subseqs t))))

(defun make-number-span (number-list)
  (labels ((scan-list (nums curr acc)
	     (arc-if (null nums)
		     (cons curr acc)
		     (null curr)
		     (scan-list (rest nums) (cons (first nums) curr) acc)
		     (= (first nums) (1+ (first curr)))
		     (scan-list (rest nums) (cons (first nums) curr) acc)
		     (scan-list nums nil (cons curr acc)))))
    (reduce #'(lambda (x y) (concatenate 'string y ", " x))
	    (mapcar #'(lambda (sublist) (if (= (list-length sublist) 1)
					    (format nil "~A" (car sublist))
					    (format nil "~A-~A" (car (last sublist)) 
						    (first sublist)))) 
		    (scan-list number-list nil nil)))))

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
		 
(defun add-parish-query (fullname shortname country state county city street
			 street-number zip phone email website diocese)
  (sql (:insert-into 'parishes :set
			 'fullname fullname
			 'shortname shortname
			 'country country
			 'state state
			 'county county
			 'city city
			 'street street
			 'street_number street-number
			 'zip zip
			 'phone (clean-phone phone)
			 'email email
			 'website website
			 'diocese diocese)))

(defun add-schedule-queries (parish sacrament-type start-time end-time
			   years months doms dows)
  (append (list (sql (:insert-into 'schedules :set
				   'parish_id parish
				   'sacrament_type (string-capitalize sacrament-type)
				   'start_time start-time
				   'end_time end-time)))
	  (mapcar #'(lambda (year) (sql (:insert-into 'schedule_year_map
						      :set 'year year)))
		  years)
	  (mapcar #'(lambda (month) (sql (:insert-into 'schedule_month_map
						      :set 'month (string-capitalize month))))
		  months)
	  (mapcar #'(lambda (dom) (sql (:insert-into 'schedule_dom_map
						      :set 'day_of_month dom)))
		  doms)
	  (mapcar #'(lambda (dow) (sql (:insert-into 'schedule_dow_map
						      :set 'day_of_week (string-downcase dow))))
		  dows)))

(defmacro call-with (function parameters lambda-list)
  `(lambda ,parameters (funcall ,function ,@lambda-list)))

(defun handle-admin-action (action)
  (arc-if (equal action "add-parish") 
	  (query (add-parish-query
		  (parameter "parish-long-name")
		  (parameter "parish-short-name")
		  (parameter "parish-country")
		  (parameter "parish-state")
		  (parameter "parish-county")
		  (parameter "parish-city")
		  (parameter "parish-street")
		  (parameter "parish-street-number")
		  (parameter "parish-zip")
		  (parameter "parish-phone")
		  (parameter "parish-email")
		  (parameter "parish-website")
		  (parameter "parish-diocese")))
	  (equal action "delete-parish")
	  (execute (:delete-from 'parishes :where (:= 'parish_id (parameter "parish"))))
	  (equal action "add-schedule")
	  (mapcar (call-with #'cl-postgres:exec-query (query) (*database* query))
		  (add-schedule-queries (parameter "parish")
					(parameter "sacrament-type")
					(parameter "start-time")
					(parameter "end-time")
					(parse-number-span (parameter "years"))
					(loop for month in months
					   when (parameter (car month))
					   collect (car month))
					(parse-number-span (parameter "days"))
					(loop for dow in dows
					   when (parameter (car dow))
					   collect (car dow))))
	  (equal action "delete-schedule")
	  (execute (:delete-from 'schedules :where (:= 'schedule_id (parameter "schedule"))))))

(defun parish-schedule-html (parish-id)
  (with-html-output-to-string (*standard-output* nil :indent t :prologue nil)
    (:table
     (dolist (schedule (query (:select 'schedule_id 'sacrament_type 'start_time 'end_time 
				       :from 'schedules
				       :where (:= 'parish_id parish-id))))
       (destructuring-bind (schedule-id sacrament-type start-time end-time) 
	   schedule
	 (htm (:tr (:td "Schedule") (:td (str schedule-id)))
	      (:tr (:td "Type") (:td (str sacrament-type)))
	      (:tr (:td "Time") (:td (str (format nil "~A-~A" start-time end-time))))
	      (aif (query (:select 'year :from 'schedule_year_map
				   :where (:= 'schedule_id schedule-id)) :column)
		(htm (:tr (:td "Years") (:td (str (reduce #'(lambda (x y)
							      (concatenate 'string x ", " y))
							  it))))))
	      (aif (query (:select 'month :from 'schedule_month_map
				   :where (:= 'schedule_id schedule-id)) :column)
		(htm (:tr (:td "Months") (:td (str (reduce #'(lambda (x y)
							       (concatenate 'string x ", " y))
							   it))))))
	      (aif (query (:select 'day_of_month :from 'schedule_dom_map
				   :where (:= 'schedule_id schedule-id)) :column)
		(htm (:tr (:td "Days of the Month") (:td (str (make-number-span it))))))
	      (aif (query (:select 'day_of_week :from 'schedule_dow_map
				   :where (:= 'schedule_id schedule-id)) :column)
		(htm (:tr (:td "Days of the Week") (:td (str (reduce #'(lambda (x y)
									 (concatenate 'string x ", " y))
								     it))))))
	      (:tr (:td
		    (:form :id "delete-schedule" :action "admin" :method "post"
			   (:input :type "hidden" :name "action" :value "delete-schedule")
			   (:input :type "hidden" :name "schedule" :value schedule-id)
			   (:input :type "submit" :value "Delete Schedule"))))))))))
  
(define-easy-handler (admin :uri "/admin" :default-request-type :post) ()
  (with-connection *connection-spec*
    (aif (parameter "action")            ;if request came from admin page form
					;dispatch on action type
      (handle-admin-action it))
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
	    (:input :type "text" :name "parish" "Parish-ID")(:br)
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
					   parish)))) 
			   (:br)
			   (str (parish-schedule-html (cdar parish)))
			   (:form :id "delete-parish" :action "admin" :method "post"
			    (:input :type "hidden" :name "action" :value "delete-parish")
			    (:input :type "hidden" :name "parish" :value (cdar parish))
			    (:input :type "submit" :value "delete"))
			   (:br))))
		  parishes))))))))
