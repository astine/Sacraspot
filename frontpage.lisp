;;; website frontpage.lisp - Andrew Stine (C) 2009

(in-package #:sacraspot)

(define-easy-handler (frontpage :uri "/" :default-request-type :post) ()
  (with-connection *connection-spec*
    (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
      (:html
       (:head 
	(:title "Sacraspot"))
       (:body
	(:h1 "Sacraspot")
	(:hr)
	(:table :style "float:left;border:1px solid #111;border-collapse:collapse"
	 (let ((lat-long (latitude-and-longitude (remote-addr*)))
	       (cell-style "border:1px solid #111"))
	   (doquery (:limit
		     (:order-by
		      (:select 'fullname 'city 'state 'sacrament_type 'time 'details
			       (:as (:ll_distance 'latitude (first lat-long) 
						  'longitude (second lat-long)) 
				    'distance)
			       (:as (:+ (:ll_distance 'latitude (first lat-long)
						      'longitude (second lat-long))
					(:/ (:extract 'epoch (:- 'time (now))) 360) )
				    'weight)
			       :from 'events
			       :inner-join 'parishes :on (:= 'events.parish_id 
							     'parishes.parish_id)
			       :inner-join 'schedules :on (:= 'events.schedule_id 
							      'schedules.schedule_id)
			       :where (:and (:< (:ll_distance 'latitude (first lat-long)
							      'longitude (second lat-long)) 
						25)
					    (:> 'time (now))))
		      'weight)
		     10)
	       (fullname city state kind time details distance weight)
	     (htm (:tr :style cell-style 
		   (:td :style cell-style (str kind))
		   (:td :style cell-style (str (format nil "~A in ~A, ~A" fullname city state)))
		   (:td :style cell-style (str (format-hr-timestamp time)))
		   (:td :style cell-style (str details))
		   ;(:td :style cell-style (str weight))
		   (:td :style cell-style (str (format nil "~R miles" (round distance))))))))))))))
