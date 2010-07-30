;;; website query-sacraments.lisp - Andrew Stine (C) 2009-2010

(in-package #:sacraspot)

;;query elements:
;ip         - ip used for geolocation, defaults to requestors ip
;time       - timestamp for searching on, defaults to 'now'
;latitude   - geolocation, defaults to what is derived from ip
;longitude  - geolocation, defaults to what is derived from ip
;distance   - in miles, search scope, defaults to 25
;future     - search scope, in seconds, defaults to 5 days (432,000 seconds)
;maxresults - maximum number of results returned, defaults to 25
;sacraments - json list of sacraments to search for, defaults to mass and confession

(defun generate-sacraments-query (time distance future maxresults sacraments language latitude longitude)
  "Returns the query used to pull lists of upcoming local sacraments"
  (sql (:limit
	(:order-by
	 (:select 'fullname 'city 'state 'sacrament_type 'time 'details 'language 'latitude 'longitude
		  (:as (:ll_distance 'latitude latitude 
				     'longitude longitude) 
		       'distance)
		  (:as (:+ (:* (:ll_distance 'latitude latitude
					     'longitude longitude)
			       6)
			   (:/ (:extract 'epoch (:- 'time time)) 360) )
		       'weight)
		  :from 'events
		  :inner-join 'parishes :on (:= 'events.parish_id 
						'parishes.parish_id)
		  :inner-join 'schedules :on (:= 'events.schedule_id 
						 'schedules.schedule_id)
		  :where (:and (:< (:ll_distance 'latitude latitude 
						 'longitude longitude) 
				   distance)
			       (:raw (sql-compile
				      `(:or
					,@(mapcar (lambda (sacrament)
						    `(:= 'sacrament_type ,sacrament))
						  sacraments))))
			       (:raw (sql-compile (if language `(:= 'language ,language) t)))
			       (:> 'time time)
			       (:< 'time (timestamp+ time future :sec))))
	 'weight)
	maxresults)))

(defun query-sacraments (time distance future maxresults sacraments language latitude longitude)
  "Returns a JSON string containing the results of query based on the constraints provided."
  (declare (type local-time:timestamp time) (type integer distance future maxresults)
	   (type list sacraments) (type float latitude longitude))
  (yason:with-output-to-string* ()
    (yason:with-array ()
      (doquery (:raw (generate-sacraments-query time distance future maxresults sacraments language latitude longitude))
	  ;(:limit
		;(:order-by
		 ;(:select 'fullname 'city 'state 'sacrament_type 'time 'details 'language 'latitude 'longitude
			  ;(:as (:ll_distance 'latitude latitude 
					     ;'longitude longitude) 
			       ;'distance)
			  ;(:as (:+ (:* (:ll_distance 'latitude latitude
						 ;'longitude longitude)
				       ;6)
				   ;(:/ (:extract 'epoch (:- 'time time)) 360) )
			       ;'weight)
			  ;:from 'events
			  ;:inner-join 'parishes :on (:= 'events.parish_id 
							;'parishes.parish_id)
			  ;:inner-join 'schedules :on (:= 'events.schedule_id 
							 ;'schedules.schedule_id)
			  ;:where (:and (:< (:ll_distance 'latitude latitude 
							 ;'longitude longitude) 
					   ;distance)
				       ;(:raw (sql-compile
					      ;`(:or
						;,@(mapcar (lambda (sacrament)
							    ;`(:= 'sacrament_type ,sacrament))
							  ;sacraments))))
				       ;(:raw (sql-compile (if language `(:= 'language ,language) t)))
				       ;(:> 'time time)
				       ;(:< 'time (timestamp+ time future :sec))))
		 ;'weight)
		;maxresults)
	  (fullname city state kind time details language latitude longitude distance weight)
	(yason:with-object ()
	  (yason:encode-object-element "fullname" fullname)
	  (yason:encode-object-element "city" city)
	  (yason:encode-object-element "state" state)
	  (yason:encode-object-element "kind" kind)
	  (yason:encode-object-element "time" (format-hr-timestamp time))
	  (yason:encode-object-element "details" details)
	  (yason:encode-object-element "language" language)
	  (yason:encode-object-element "latitude" latitude)
	  (yason:encode-object-element "longitude" longitude)
	  (yason:encode-object-element "distance" distance)
	  (yason:encode-object-element "weight"  weight))))))

(defun query-sacraments-html (time distance future maxresults sacraments language latitude longitude)
  "Returns a HTML string containing the results of query based on the constraints provided."
  (declare (type local-time:timestamp time) (type integer distance future maxresults)
	   (type list sacraments) (type float latitude longitude))
  (with-html-output-to-string (*standard-output*)
    (:table :id "sacraments" :class "sacraments-table"
      (doquery (:raw (generate-sacraments-query time distance future maxresults sacraments language latitude longitude))
	  (fullname city state kind time details language latitude longitude distance weight)
	(htm (:tr
	       (:td (str kind))
	       (:td (str fullname))
	       (:td (str (format nil "~A, ~A" city state)))
	       (:td (str (format-hr-timestamp time)))
	       (:td (str details))
	       (:td (str (format nil "~R miles" (round distance))))))))))

(define-easy-handler (query-sacraments* :uri "/query-sacraments" :default-request-type :post) ()
  "Handles calls to query-sacraments"
  (with-connection *connection-spec*
    (let ((ip (fetch-parameter "ip" (remote-addr*) nil))
	  (time (fetch-parameter "time" (now) #'parse-timestring))
	  (distance (fetch-parameter "distance" 25))
	  (future (fetch-parameter "future" 453000))
	  (maxresults (fetch-parameter "maxresults" 25))
	  (sacraments (fetch-parameter "sacraments" '("Mass" "Confession") #'yason:parse))
	  (language (fetch-parameter "language")))
      (let* ((lat-long (unless (and (parameter "latitude")
				    (parameter "longitude"))
			 (latitude-and-longitude ip)))
	     (latitude (fetch-parameter "latitude" (first lat-long)))
	     (longitude (fetch-parameter "longitude" (second lat-long)))
	     )
	;lat-long))))
	;(write-to-string (list time distance future maxresults sacraments latitude longitude))))))
	(query-sacraments time distance future maxresults sacraments language latitude longitude)
))))

(define-easy-handler (query-sacraments-html* :uri "/query-sacraments-html" :default-request-type :post) ()
  "Handles calls to query-sacraments-html"
  (with-connection *connection-spec*
    (let ((ip (fetch-parameter "ip" (remote-addr*) nil))
	  (time (fetch-parameter "time" (now) #'parse-timestring))
	  (distance (fetch-parameter "distance" 25))
	  (future (fetch-parameter "future" 453000))
	  (maxresults (fetch-parameter "maxresults" 25))
	  (sacraments (fetch-parameter "sacraments" '("Mass" "Confession") #'yason:parse))
	  (language (fetch-parameter "language")))
      (let* ((lat-long (unless (and (parameter "latitude")
				    (parameter "longitude"))
			 (latitude-and-longitude ip)))
	     (latitude (fetch-parameter "latitude" (first lat-long)))
	     (longitude (fetch-parameter "longitude" (second lat-long))))
	(let ((*string-modifier* #'identity))
	  (with-output-to-string (*standard-output*)
	    (fill-and-print-template (pathname "/var/www/localhost/htdocs/www/fallback-frontpage.html")
				     `(:sacraments ,(query-sacraments-html time distance future maxresults sacraments language latitude longitude))
				     :stream *standard-output*)))))))
