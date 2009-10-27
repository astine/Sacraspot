;;; website select-parishes.lisp - Andrew Stine (C) 2009

(in-package #:sacraspot)

;;query elements:
;fullname
;shortname
;country
;state
;street
;street number
;zip code
;phone number
;email
;website
;latitude
;longitude
;diocese

(defmacro defselect (table-name &rest columns)
  `(defun ,(intern (concatenate 'string "GET-" (symbol-name table-name))) ,columns
     (doquery (sql-compile (list :select ,@(mapcar #'(lambda (column)
						       `(quote ,column))
						   columns) :from ,`(quote ,table-name) :where
				 (list :or ,@(mapcar (lambda (column)
						   (list 'if column (list 'list := `(quote ,column) 
									  column)))
						columns))))
	 ,columns ,@(mapcar (lambda (column)
			      `(print ,column))
			    columns))))

(defun get-parishes (fullname shortname diocese
		     country state city street street-number zip
		     phone email website
		     latitude longitude)
  (doquery (sql-compile `(:select fullname shortname country state city
				  street street_number zip phone email
				  website latitude longitude
				  :from parishes :where
				  (:or ,(if fullname `(:= fullname ,fullname))
				       ,(if shortname `(:= shortname ,shortname))
				       ,(if country `(:= country ,country))
				       ,(if state `(:= state ,state))
				       ,(if city `(:= city ,city))
				       ,(if street `(:= street ,street))
				       ,(if street-number `(:= street_number ,street-number))
				       ,(if zip `(:= zip ,zip))
				       ,(if phone `(:= phone ,(clean-phone phone)))
				       ,(if email `(:= email ,email))
				       ,(if website `(:= website ,website))
				       ,(if latitude `(:= latitude ,latitude))
				       ,(if longitude `(:= longitude ,longitude))
				       ,(if diocese `(:= diocese ,diocese)))))
      (fullname shortname country state city street street-number zip phone email website latitude longitude)
