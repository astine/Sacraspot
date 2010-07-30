;;; website ip-geolocation.lisp - Andrew Stine (C) 2009-2010

(in-package #:sacraspot)

;;; Functions to get approximate lat/long coordinates based on
;;; IP addresses.
;;; uses the ip-blocks database

(defvar *geolocus-db-changed* nil)

(defun normalize-ip (ip-address)
  "Converts and ip address to a single integer"
  (declare (type string ip-address))
  (multiple-value-bind (whole-match fields)
      (scan-to-strings "([0-9]*)\.([0-9]*)\.([0-9]*)\.([0-9]*)" ip-address)
    (setq fields (map 'list #'read-from-string fields))
    (unless (and (equal whole-match ip-address)
		 (every (lambda (num) (<= 0 num 255)) fields))
      (error "bad ip: ~a" ip-address))
    (+ (* (elt fields 0) 16777216)
       (* (elt fields 1) 65536)
       (* (elt fields 2) 256)
       (elt fields 3))))

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
  (declare (type string ip-address))
  (aif (get-locale ip-address)
    (list (getf it :latitude)
	  (getf it :longitude))
    (warn "No latitude or longitude found for ip ~A" ip-address)))

(define-easy-handler (latitude-and-longitude* :uri "/latitude-and-longitude" :default-request-type :post) ()
  "Returns the latitude and longitude as to clients in JSON format
   Client can either provide an IP address as a parameter or the client's own
   IP will be used."
  (with-connection *connection-spec*
    (destructuring-bind (latitude longitude)
	(latitude-and-longitude (fetch-parameter "ip" (remote-addr*) nil))
      (yason:with-output-to-string* ()
	(yason:with-object ()
	  (yason:encode-object-element "latitude" latitude)
	  (yason:encode-object-element "longitude" longitude))))))


; other thoughts:
;
; This is how sacraspot guesses the latitude and longitude of a user if not told by the client
; It might be useful to have some code for querying google here or for caching more accurate 
; locations
;
; 07/28/2010
