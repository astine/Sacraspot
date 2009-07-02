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
  
(defmacro aif (condition &body body)
  `(let ((it ,condition))
     (if it ,@body)))

(defmacro awhile (condition &body body)
  `(do ((it ,condition ,condition)) 
       ((not it)) ,@body))

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

(define-easy-handler (admin :uri "/admin" :default-request-type :post) ()
  (with-connection *connection-spec*
    (if (parameter "action")            ;if request came from admin page form
					;dispatch on action type
	(cond ((equal (parameter "action") "add") 
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
	      ((equal (parameter "action") "delete")
	       (execute (:delete-from 'parishes :where (:= 'parish_id (parameter "parish")))))))
    (let ((parishes (query (:select '* :from 'parishes) :alists)))
      (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
	(:html
	 (:head 
	  (:title "admin"))
	 (:body
	  (:form :id "add-parish" :action "admin" :method "post"
	   (:input :type "hidden" :name "action" :value "add")
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
	  (mapcar #'(lambda (parish)
		      (htm (:div :style "float:left;"
			    (:table
			     (fmt "~{<tr><td>~A</td><td>~A</td></tr>~}" 
				  (alist-to-plist 
				   (mapcar #'(lambda (pair)
					       (if (equal (symbol-name (first pair))
							  "PHONE")
						   (cons (car pair) (pretty-print-phone (cdr pair)))
						   pair))
					   (rest parish))))) 
			   (:br)
			   (:form :id "delete-parish" :action "admin" :method "post"
			    (:input :type "hidden" :name "action" :value "delete")
			    (:input :type "hidden" :name "parish" :value (cdar parish)
			    (:input :type "submit" :value "delete")))
			   (:br))))
		  parishes)))))))

