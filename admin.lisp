;;; website admin.lisp - Andrew Stine (C) 2009

(in-package #:sacraspot)

;; constants and parameters:
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

;; utilities
(defun alist-to-plist (alist)
  (mapcan #'(lambda (pair) 
	      (list (car pair) (cdr pair)))
	  alist))

(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar #'(lambda (symbol)
		     `(,symbol (gensym)))
		 symbols)
     ,@body))

(defmacro defcached (name lambda-list &body body)
  "Defines a function with a cache that dispatches on its argument list."
  (with-gensyms (table value found)
    (let ((params (loop for param in lambda-list
		       when (not (or (eql param '&optional)
				     (eql param '&keys)
				     (eql param '&rest)
				     (eql param '&body)))
		       collect (if (listp param) (car param) param))))
      `(let ((,table (make-hash-table :test #'equal)))
	 (defun ,name ,lambda-list
	   (multiple-value-bind (,value ,found)
	       (gethash (list ,@params) ,table)
	     (if ,found
		 ,value
		 (setf (gethash (list ,@params) ,table)
		       (progn ,@body)))))))))

(eval-when (:compile-toplevel)
  (defun group (list n)
    "Partitions a list into a list of sublists of length 'n'"
    (cond ((null list)
	   nil)
	  ((<= (list-length list) n)
	   (list list))
	  (t
	   (cons (subseq list 0 n)
		 (group (subseq list n) n))))))
  
(defmacro aif (condition &body body)
  "anaphoric if"
  `(let ((it ,condition))
     (if it ,@body)))

(defmacro awhile (condition &body body)
  "anaphoric while"
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
  "Returns a list of number between 'begin' and 'end'"
  (arc-if (and (listp begin)
	       (null end))
	  `(apply #'get-range ,begin)
	  (and (integerp begin)
	       (integerp end))
	  `(list ,@(get-range begin end))
	  `(funcall #'get-range ,begin ,end)))

(defun make-set (list &optional (eql-test #'=) (predicate #'<))
  "sorts a list and filters out duplicates"
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
  "Take a string of the form '1-3, 5, 8-10', and returns an order
   list of  every number, represented by the string"
  (make-set
   (mapcan #'(lambda (part)
	       (arc-if (cl-ppcre:scan "[0-9]+-[0-9]+" part)
		       (range (mapcar #'read-from-string
				      (split-sequence:split-sequence #\- part)))
		       (cl-ppcre:scan "[0-9]+" part)
		       (list (read-from-string part))))
	   (split-sequence:split-sequence #\, span :remove-empty-subseqs t))))

(defun make-number-span (number-list)
  "Takes a list of numbers and returns a string of the form:
   '1-3, 5, 8-10', representing the numbers in the list"
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
		    (scan-list (make-set number-list) nil nil)))))

(defun clean-phone (number)
  "Reduces a phone number to a string of numerals"
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

(defmacro call-with (function parameters lambda-list)
  `(lambda ,parameters (funcall ,function ,@lambda-list)))

;; main functions
(defun add-parish (fullname shortname country state city street street-number 
		   zip phone email website latitude longitude diocese)
  "Adds a parish entry with the provided fields into the 'parishes' table."
  ;validate info
  (unless (or (null email)
	      (equal "" email)
	      (scan "[a-zA-Z0-9_.-]+@[a-zA-Z0-9_.-]+[.].+" email))
    (error "Bad email address being added to parish table"))
  (unless (= (length (clean-phone phone)) 10)
    (error "Bad phone number being added to parish table"))
  (execute (:insert-into 'parishes :set
			 'fullname fullname
			 'shortname shortname
			 'country country
			 'state state
			 'city city
			 'street street
			 'street_number street-number
			 'zip zip
			 'phone (clean-phone phone)
			 'email email
			 'website website
			 'latitude latitude
			 'longitude longitude
			 'diocese diocese)))

(defun add-schedule (parish sacrament-type start-time end-time details
		     years months doms dows)
  "Adds a schedule entry to the 'schedules table as well as entries to the 
   subordinate year, month, day-of-month, and day of week tables"
  (with-transaction ()
    (execute (:insert-into 'schedules :set
			   'parish_id parish
			   'sacrament_type (string-capitalize sacrament-type)
			   'start_time start-time
			   'end_time end-time
			   'details details))
    (dolist (year years)
      (execute (:insert-into 'schedule_year_map :set 'year year)))
    (dolist (month months)
      (execute (:insert-into 'schedule_month_map :set 'month (string-capitalize month))))
    (dolist (dom doms)
      (execute (:insert-into 'schedule_dom_map :set 'day_of_month dom)))
    (dolist (dow dows)
      (execute (:insert-into 'schedule_dow_map :set 'day_of_week (string-downcase dow))))))

(defun handle-admin-action (action)
  "When a form is submited on the admin page, this function is called
   to handle the details"
  (arc-if (equal action "add-parish") 
	  (add-parish (parameter "parish-long-name")
		      (parameter "parish-short-name")
		      (parameter "parish-country")
		      (parameter "parish-state")
		      (parameter "parish-city")
		      (parameter "parish-street")
		      (parameter "parish-street-number")
		      (parameter "parish-zip")
		      (parameter "parish-phone")
		      (parameter "parish-email")
		      (parameter "parish-website")
		      (parameter "parish-latitude")
		      (parameter "parish-longitude")
		      (parameter "parish-diocese"))
	  (equal action "delete-parish")
	  (execute (:delete-from 'parishes :where (:= 'parish_id (parameter "parish"))))
	  (equal action "add-schedule")
	  (add-schedule (parameter "parish")
			(parameter "sacrament-type")
			(parameter "start-time")
			(parameter "end-time")
			(parameter "details")
			(parse-number-span (parameter "years"))
			(loop for month in months
			   when (parameter (car month))
			   collect (car month))
			(parse-number-span (parameter "days"))
			(loop for dow in dows
			   when (parameter (car dow))
			   collect (car dow)))
	  (equal action "delete-schedule")
	  (execute (:delete-from 'schedules :where (:= 'schedule_id (parameter "schedule"))))))

(defun format-time-of-day (time)
  (format-timestring nil time
		     :format '(:hour12 ":" (:min 2) " " :ampm)))

(defun parish-schedule-html (parish-id &optional style)
  "Generates html to display out a parishes schedules"
  (with-html-output-to-string (*standard-output* nil :indent t :prologue nil)
    (:table :style style
     (dolist (schedule (query (:select 'schedule_id 'sacrament_type 'start_time 'end_time 'details
				       :from 'schedules
				       :where (:= 'parish_id parish-id))))
       (destructuring-bind (schedule-id sacrament-type start-time end-time details) 
	   schedule
	 (htm (:tr (:td "Schedule") (:td (str schedule-id)))
	      (:tr (:td "Type") (:td (str sacrament-type)))
	      (:tr (:td "Time") (:td (str (format nil "~A - ~A" 
						  (format-time-of-day start-time)
						  (format-time-of-day end-time)))))
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
	      (unless (or (null details) (equal details :NULL) (equal details ""))
		  (htm (:tr (:td "Details") (:td (str details)))))
	      (:tr (:td
		    (:form :id "delete-schedule" :action "admin" :method "post"
			   (:input :type "hidden" :name "action" :value "delete-schedule")
			   (:input :type "hidden" :name "schedule" :value schedule-id)
			   (:input :type "submit" :value "Delete Schedule"))))))))))

(defcached add-parish-html (&optional style)
  (with-html-output-to-string (*standard-output* nil :indent t :prologue nil)
    (:form :id "add-parish" :action "admin" :method "post" :style style 
     (:input :type "hidden" :name "action" :value "add-parish")
     (:input :type "text" :name "parish-long-name" "parish-long-name")(:br)
     (:input :type "text" :name "parish-short-name" "parish-short-name")(:br)
     (:select :name "parish-country" (:option :value "US" "United States")) (:br)
     (:select :name "parish-state" 
	      (mapcar #'(lambda (state) 
			  (htm (:option :value (car state) 
					(str (cdr state)))))
		      states)) (:br)
     (:input :type "text" :name "parish-city" "parish-city")(:br)
     (:input :type "text" :name "parish-street" "parish-street")(:br)
     (:input :type "text" :name "parish-street-number" "parish-street-number")(:br)
     (:input :type "text" :name "parish-zip" "parish-zip")(:br)
     (:input :type "text" :name "parish-phone" "parish-phone")(:br)
     (:input :type "text" :name "parish-email" "parish-email")(:br)
     (:input :type "text" :name "parish-website" "parish-website")(:br)
     (:input :type "text" :name "parish-diocese" "parish-diocese")(:br)
     (:input :type "text" :name "parish-latitude" "parish-latitude")(:br)
     (:input :type "text" :name "parish-longitude" "parish-longitude")(:br)
     (:input :type "submit" :value "submit"))))

(defcached add-schedule-html (&optional style)
  (with-html-output-to-string (*standard-output* nil :indent t :prologue nil)
    (:form :id "add-schedule" :action "admin" :method "post" :style style 
     (:input :type "hidden" :name "action" :value "add-schedule")
     (:input :type "text" :name "parish" "Parish-ID")(:br)
     (:select :name "sacrament-type" "Sacrament Kind"
	      (:option :value "mass" "Mass")
	      (:option :value "confession" "Confession"))(:br)
     (:input :type "text" :name "start-time" "Start Time")(:br)
     (:input :type "text" :name "end-time" "End Time")(:br)
     (:input :type "text" :name "details" "Details")(:br)
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
     (:input :type "submit" :value "submit"))))

(defun format-hr-timestamp (time)
  (format-timestring nil time
		     :format '(:short-month " " :day ", " :year " ":hour12 ":" (:min 2) " " :ampm)))

(defun current-events-html (&optional style cell-style)
  (with-html-output-to-string (*standard-output* nil :indent t :prologue nil)
    (:table :style style
     (doquery (:order-by
	       (:select 'fullname 'city 'state 'sacrament_type 'time 
			:from 'events
			:inner-join 'parishes :on (:= 'events.parish_id 'parishes.parish_id)
			:inner-join 'schedules :on (:= 'events.schedule_id 'schedules.schedule_id))
	       'time)
	 (fullname city state kind time)
       (htm (:tr :style cell-style 
	     (:td :style cell-style (str kind))
	     (:td :style cell-style (str (format nil "~A in ~A, ~A" fullname city state)))
	     (:td :style cell-style (str (format-hr-timestamp time)))))))))
  
(define-easy-handler (admin :uri "/admin" :default-request-type :post) ()
  "The administration page. This page is to allow adminstrators to view the contents of the
   database and to manuall add and remove parishes and schedules."
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
	   (str (add-parish-html "float:left"))
	   (str (add-schedule-html "float:left"))
	   (str (current-events-html "float:left;border:1px solid #111;border-collapse:collapse" 
				     "border:1px solid #111")))
	  (:div :style "clear:left"
	  (mapcar #'(lambda (parish)
		      (htm (:div :style "float:left;border:1px solid #111"
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
