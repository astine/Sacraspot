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
    (unless (= (length clean-phone) 10)
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
			   'phone clean-phone
			   'email email
			   'website website
			   'latitude latitude
			   'longitude longitude
			   'diocese diocese))))

(define-easy-handler (insert-parishes* :uri "/insert-parishes" :default-request-type :post) ()
  (with-connection *connection-spec*
    ;(write-to-string (length (delete '("") (fetch-parameter "parishes" nil #'parse-csv) :test #'equal)))))
    (dolist (parish (delete '("") (fetch-parameter "parishes" nil #'parse-csv) :test #'equal))
      (apply #'insert-parish parish)))
  "worked")
