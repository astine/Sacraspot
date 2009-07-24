;;; website tests.lisp - Andrew Stine (C) 2009

(test number-span
	     (let ((number-span-string "1-3, 5, 8-10")
		   (number-span-list '(1 2 3 5 8 9 10)))
	       (is (equal number-span-string 
			  (make-number-span number-span-list)))
	       (is (equal number-span-list 
			  (parse-number-span number-span-string)))
	       (is (equal number-span-string 
			  (make-number-span (parse-number-span number-span-string))))
	       (is (equal number-span-list 
			  (parse-number-span (make-number-span number-span-list))))))

(test phone-parser
  (let ((number-1 "(703) 256-6478")
	(number-2 "(703) 586-3795"))
    (is (equal number-1 (pretty-print-phone (clean-phone number-1))))
    (is (equal number-2 (pretty-print-phone (clean-phone number-2))))))

