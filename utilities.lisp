;;; website utilities.lisp - Andrew Stine (C) 2009

(in-package #:sacraspot)

(defun alist-to-plist (alist)
  (mapcan #'(lambda (pair) 
	      (list (car pair) (cdr pair)))
	  alist))

(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar #'(lambda (symbol)
		     `(,symbol (gensym)))
		 symbols)
     ,@body))

(defmacro defcached (name lambda-list &body body)
  "Defines a function with a cache that dispatches on its argument list."
  (with-gensyms (table value found)
    (let ((params (loop for param in lambda-list
		       when (not (or (eql param '&optional)
				     (eql param '&keys)
				     (eql param '&rest)
				     (eql param '&body)))
		       collect (if (listp param) (car param) param))))
      `(let ((,table (make-hash-table :test #'equal)))
	 (defun ,name ,lambda-list
	   (multiple-value-bind (,value ,found)
	       (gethash (list ,@params) ,table)
	     (if ,found
		 ,value
		 (setf (gethash (list ,@params) ,table)
		       (progn ,@body)))))))))

(eval-when (:compile-toplevel :load-toplevel)
  (defun group (list n)
    "Partitions a list into a list of sublists of length 'n'"
    (cond ((null list)
	   nil)
	  ((<= (list-length list) n)
	   (list list))
	  (t
	   (cons (subseq list 0 n)
		 (group (subseq list n) n))))))
  
(defmacro aif (condition &body body)
  "anaphoric if"
  `(let ((it ,condition))
     (if it ,@body)))

(defmacro awhile (condition &body body)
  "anaphoric while"
  `(do ((it ,condition ,condition)) 
       ((not it)) ,@body))

(defmacro arc-if (&body forms)
  `(cond ,@(mapcar #'(lambda (clause)
		       (if (= (list-length clause) 2)
			   clause
			   (cons 't clause)))
		   (sacraspot::group forms 2))))

(defun get-range (begin end)
  (if (<= begin end)
      (cons begin (get-range (1+ begin) end))))

(defmacro range (begin &optional end)
  "Returns a list of number between 'begin' and 'end'"
  (arc-if (and (listp begin)
	       (null end))
	  `(apply #'get-range ,begin)
	  (and (integerp begin)
	       (integerp end))
	  `(list ,@(get-range begin end))
	  `(funcall #'get-range ,begin ,end)))

(defun make-set (list &optional (eql-test #'=) (predicate #'<))
  "sorts a list and filters out duplicates"
  (labels ((filter-dups (lst)
	     (arc-if (null lst)
		     nil
		     (equal (list-length lst) 1)
		     lst
		     (funcall eql-test (first lst) (second lst))
		     (filter-dups (rest lst))
		     (cons (first lst) (filter-dups (rest lst))))))
    (filter-dups (sort list predicate))))

(defmacro call-with (function parameters lambda-list)
  `(lambda ,parameters (funcall ,function ,@lambda-list)))

