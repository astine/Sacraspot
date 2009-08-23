;;; website ip-geolocation.lisp - Andrew Stine (C) 2009

(in-package #:sacraspot)

(defvar *geolocus-db-changed* nil)

(defun normalize-ip (ip-address)
  (multiple-value-bind (whole-match fields)
      (scan-to-strings "([0-9]*)\.([0-9]*)\.([0-9]*)\.([0-9]*)" ip-address)
    (unless (equal whole-match ip-address)
      (error "bad ip"))
    (+ (* (read-from-string (elt fields 0)) 16777216)
       (* (read-from-string (elt fields 1)) 65536)
       (* (read-from-string (elt fields 2)) 256)
       (read-from-string (elt fields 3)))))

(defcached get-locale (:flag *geolocus-db-changed* :flag-auto-unset t)
    (ip-address)
  (let ((ip-num (normalize-ip ip-address)))
    (alist-to-plist
     (query (:select 'country 'region 'city 'postalcode 'latitude 'longitude 'metrocode 'areacode
		     :from 'ip_blocks
		     :inner-join 'ip_locations :on (:= 'ip_blocks.locid 'ip_locations.locid)
		     :where (:and (:>= ip-num 'startipnum)
				  (:<= ip-num 'endipnum)))
	    :alist))))

(defun latitude-and-longitude (ip-address)
  (let ((locale (get-locale ip-address)))
    (if locale
	(list (getf locale :latitude)
	      (getf locale :longitude))
	(list 38.7705012 -77.4287641))))


	       
