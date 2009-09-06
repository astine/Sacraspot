(in-package #:sacrapspot)

;;convenience macros for inserting javascript in page headers
(defmacro ps-script (&body forms)
  "Simple macro to wrap parenscript elements with proper tags"
  `(with-html-output-to-string (*standard-output* nil :prologue nil)
     (:script :type "text/javascript"
      (fmt "~%// <![CDATA[~%~A~%// ]]>~%" (ps ,@forms)))))

(defmacro ps-script* (&body forms)
  "Simple macro to wrap parenscript elements with proper tags"
  `(with-html-output-to-string (*standard-output* nil :prologue nil)
     (:script :type "text/javascript"
      (fmt "~%// <![CDATA[~%~A~%// ]]>~%" (ps* ,@forms)))))

(defvar *callback-signifier* 'cb)
(defvar *asynchronous-block-signifier* '(async))

(defpsmacro async-bind (l-list calling-form &rest body)
  "Allows a user to call a function that requires a callback,
   and bind the callback data to the function"
  (labels ((search-tree (tree)
	     (mapcar #'(lambda (sub-form)
			 (cond ((listp sub-form)
				(search-tree sub-form))
			       ((equal sub-form *callback-signifier*)
				`(lambda ,l-list ,@body))
			       (t sub-form)))
		     tree)))
    (search-tree calling-form)))

(defun filter (tree test replacement)
  (let ((old-tree nil))
    (labels ((check-node (node)
	       (cond ((null node) nil)
		     ((funcall test node) 
		      (push node old-tree)
		      (if (functionp replacement)
			  (funcall replacement)
			  replacement))
		     ((atom node) node)
		     ((consp node)
		      (cons (check-node (car node))
			    (check-node (cdr node)))))))
      (values (check-node tree)
	      (nreverse old-tree)))))

(defpsmacro async-transform (&rest forms)
  "transforms a closure, containing asynchronous calls to
   a proper javascript with callbacks"
  (let ((callback-bindings nil))
    (multiple-value-bind (callback async-callers)
	(filter (car forms) 
		#'(lambda (node)
		    (and (listp node)
			 (not (null (find (car node) *asynchronous-block-signifier*)))))
		#'(lambda ()
		    (push (ps-gensym) callback-bindings)
		    (car callback-bindings)))
      (labels ((wrap-callers (callers bindings)
		 (cond ((and (null callers) (cdr forms))
			`(progn ,callback
				(async-transform ,@(cdr forms))))
		       ((null callers)
			callback)
		       (t
			`(async-bind (,(car bindings)) ,(car callers)
				     ,(wrap-callers (cdr callers) (cdr bindings)))))))
	(wrap-callers async-callers
		      (nreverse callback-bindings))))))

(defpsmacro async (form)
  form)

(defpsmacro async-defun (name l-list &rest body)
  (push name *asynchronous-block-signifier*)
  `(defun ,name ,l-list ,@body))

(defpsmacro with-async ((&key (return 'false))
			&rest body)
  `(progn (async-transform ,@body)
	  (return ,return)))

(define-easy-handler (maps-testing :uri "/bulk" :default-request-type :post) ()
  (aif (parameter "addy")
    (dolist (entry (mapcar #'(lambda (line)
			       (split-sequence #\, line))
			   (split-sequence #\newline it)))
      (execute (
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html
     (:head
      (:script :type "text/javascript" :src "http://maps.google.com/maps/api/js?sensor=true")
      (str (ps-script*
	`(let ((geocoder nil)
	      (map nil))
	  (defun initialize ()
	    (setf geocoder (new (google.maps.*Geocoder)))
	    (let* ((latlng (new (google.maps.*lat-lng -34.397 150.644)))
		  (my-options (create :zoom 8 
				      :center latlng 
				      :map-type-id google.maps.*map-type-id.*r-o-a-d-m-a-p)))
	      (setf map (new (google.maps.*map (document.get-element-by-id "map_canvas") my-options)))))
	  
	  (defun get_address_component(component_list component_name)
	    (dolist (component component_list)
	      (dolist (type component.types)
		(if (== type component_name)
		    (return component.long_name))))
	    (return ""))
	  
	  (defun write_address (address)
	    (let ((components address.address_components))
	      (+= document.form.addy.value (+ (slot-value (document.get-element-by-id "long-name") 'value) ","))
	      (+= document.form.addy.value (+ (slot-value (document.get-element-by-id "short-name") 'value) ","))
	      (dolist (component_name (array "country" "administrative_area_level_1"
					     "locality" "route" "street_number"
					     "postal_code"))
		(+= document.form.addy.value (+ (get_address_component components component_name) ",")))
	      (+= document.form.addy.value (+ (slot-value (document.get-element-by-id "phone") 'value) ","))
	      (+= document.form.addy.value (+ (slot-value (document.get-element-by-id "email") 'value) ","))
	      (+= document.form.addy.value (+ (slot-value (document.get-element-by-id "website") 'value) ","))
	      (+= document.form.addy.value (+ (address.geometry.location.lat) ","))
	      (+= document.form.addy.value (+ (address.geometry.location.lng) ,(string #\newline)))
	      (+= document.form.addy.value (+ (slot-value (document.get-element-by-id "diocese") 'value) ","))))
	  
	  (defun writeaddy (address)
	    (setf document.form.addy.value ( + document.form.addy.value addy ",")))
	  
	  (defun code-address ()
	    (with-async ()
	      (let* ((address (slot-value (document.get-element-by-id "address") 'value)))
		(if geocoder
		    (geocoder.geocode (create "address" address) 
				      (lambda (results status)
					(if (== status google.maps.*geocoder-status.*o-k)
					    (if (!= status google.maps.*geocoder-status.*z-e-r-o_-r-e-s-u-l-t-s)
						(progn
						  (map.set_center (slot-value (aref results 0) 'geometry.location))
						  (let* ((marker (new (google.maps.*marker 
								       (create :map map 
									       :position (slot-value (aref results 0) 'geometry.location)))))
							 (addy_cmps (slot-value (aref results 0) 'address_components)))
						    (write_address (aref results 0))))
						(alert "No results were found"))
					    (alert (+ "Geocode not successful for the following reasons: " status)))))))))))))
    (:body :onload "initialize()"
     (:div
      "Parish Long Name"
      (:input :type "textbox" :size "40" :id "long-name" :value "")
      (:br)
      "Parish Short Name"
      (:input :type "textbox" :size "40" :id "short-name" :value "")
      (:br)
      "Address"
      (:input :type "textbox" :size "60" :id "address" :value "1600 Ampitheatre Pky, Mountain View, CA" )
      (:br)
      "Diocese"
      (:input :type "textbox" :size "40" :id "diocese" :value "")
      (:br)
      "Phone"
      (:input :type "textbox" :size "40" :id "phone" :value "")
      (:br)
      "Website"
      (:input :type "textbox" :size "40" :id "website" :value "")
      (:br)
      "Email"
      (:input :type "textbox" :size "40" :id "email" :value "")
      (:br)
      (:input :type "submit" :onclick "codeAddress()"))
     (:div :id "map_canvas" :style "width: 250px; height: 150px")
     (:form :action "" :method "" :name "form"
      (:textarea :name "addy" :id "addy" :rows "20" :cols "100")
      (:input :type "submit" :value "Add Parishes"))))))
						 
						       
