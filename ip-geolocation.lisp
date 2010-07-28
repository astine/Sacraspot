;;; website ip-geolocation.lisp - Andrew Stine (C) 2009-2010

(in-package #:sacraspot)

;;; Functions to get approximate lat/long coordinates based on
;;; IP addresses.
;;; uses the ip-blocks database

(defvar *geolocus-db-changed* nil)

(defun normalize-ip (ip-address)
  "Converts and ip address to a single integer"
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
  "Returns the latitude and longitude as stored for a given ip address"
  (let ((locale (get-locale ip-address)))
    (if locale
	(list (getf locale :latitude)
	      (getf locale :longitude))
	(list 38.7705012 -77.4287641)))) ;FIXME: should do something about this default


; other thoughts:
;
; This is how sacraspot guesses the latitude and longitude of a user if not told by the client
; It might be useful to have some code for querying google here or for caching more accurate 
; locations
;
; 07/28/2010
