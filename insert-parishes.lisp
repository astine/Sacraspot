;;; website insert-parishes.lisp - Andrew Stine (C) 2009

(in-package #:sacraspot)

(defun insert-parish (fullname shortname country state city street street-number 
		      zip phone email website latitude longitude diocese)
  "Adds a parish entry with the provided fields into the 'parishes' table."
  (let ((clean-phone (clean-phone phone)))
					;validate info
    (unless (or (null email)
		(equal "" email)
		(scan "[a-zA-Z0-9_.-]+@[a-zA-Z0-9_.-]+[.].+" email))
      (error "Bad email address being added to parish table"))
    (unless (or (null clean-phone)
		(equal "" clean-phone)
		(= (length clean-phone) 10))
      (error "Bad phone number being added to parish table"))
    (execute (:insert-into 'parishes :set
			   'fullname fullname
			   'shortname shortname
			   'country country
			   'state state
			   'city city
			   'street street
			   'street_number (if (equal street-number "") "0" street-number)
			   'zip zip
			   'phone clean-phone
			   'email email
			   'website website
			   'latitude latitude
			   'longitude longitude
			   'diocese diocese))))

(define-easy-handler (insert-parishes :uri "/insert-parishes" :default-request-type :post) ()
  (let ((parishes (fetch-parameter "parishes" nil #'parse-csv)))
    (with-connection *connection-spec*
      (dolist (parish parishes)
	(apply #'insert-parish parish)))))
