;;; website caching.lisp - Andrew Stine (C) 2009

(in-package #:sacraspot)

(defmacro cached-lambda ((lambda-list &key timeout flag flag-auto-unset frequency no-dispatch-params ) 
			 &body body)
  "Returns a function with a cache that dispatches on its argument list."
  (with-gensyms (table value found)
    (let ((params (loop for param in lambda-list
		       when (not (or (eql param '&optional)
				     (eql param '&keys)
				     (eql param '&rest)
				     (eql param '&body)))
		       collect (if (listp param) (car param) param))))
      `(let* ,(append `((,table (make-hash-table :test #'equal)))
		      (if frequency `((count ,(round (/ 1 frequency)))))
		      (if frequency `((counter count)))
		      (if timeout `((time ,(get-universal-time)))))
	 (lambda ,lambda-list
	   (multiple-value-bind (,value ,found)
	       (gethash (list ,@params) ,table)
	     ,(if frequency `(incf counter))
	     (if (and ,(if no-dispatch-params
			   `t
			   found)
		      ,(if timeout
			   `(> (+ time ,timeout)
			       (get-internal-real-time))
			   't)
		      ,(if flag 
			   `(not ,flag)
			   't)
		      ,(if frequency
			   `(>= count counter)
			   't))
		 ,value
		 (setf (gethash (list ,@params) ,table)
		       (progn 
			 ,(if timeout
			      `(setf time (get-internal-real-time)))
			 ,@(when flag-auto-unset 
				 `((if ,flag (clrhash ,table))
				   (setf ,flag nil)))
			 ,(if frequency
			      `(setf counter 0))
			 ,@body)))))))))

(defmacro with-cache ((lambda-list &key timeout flag flag-auto-unset frequency no-dispatch-params) &body body)
  "Creates a closure with a local cache that dispatches on a list
   of passed variables"
  (let ((cache (eval `(cached-lambda (,lambda-list :timeout ,timeout 
						   :flag ,flag 
						   :flag-auto-unset ,flag-auto-unset 
						   :frequency ,frequency 
						   :no-dispatch-params ,no-dispatch-params) 
			,@body))))
    `(apply ,cache (list ,@lambda-list))))

(defmacro defcached (name (&key timeout flag flag-auto-unset frequency no-dispatch-params) lambda-list &body body)
  "Defines a function with a cache that dispatches on its argument list."
  `(setf (symbol-function (quote ,name))
	 (cached-lambda (,lambda-list :timeout ,timeout 
				      :flag ,flag 
				      :flag-auto-unset ,flag-auto-unset 
				      :frequency ,frequency
				      :no-dispatch-params ,no-dispatch-params) 
	   ,@body)))

(defmacro defvar-gensym (&optional val doc)
  `(defvar ,(gensym) ,val ,doc))
