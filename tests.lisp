;;; website sacraspot.asd - Andrew Stine (c) 2009-2010

(in-package #:sacraspot-tests)

;;; The test suite or sacraspot
;;; This is extremely basic and actually insufficient for now.

(def-suite sacraspot-tests
    :description "The test suite for sacraspot")

(in-suite sacraspot-tests)

(test alist-to-plist-test
  (is (equal (alist-to-plist '((:a . 1) (:b . 2) (:c . 3)))
	     '(:a 1 :b 2 :c 3))))

(test number-spans
  (is (equal (parse-number-span "1-3, 4, 6-8")
	     '(1 2 3 4 6 7 8)))
  (is (equal (make-number-span '(1 3 4 5 7))
	     "1, 3-5, 7"))
  )

(test phone-numbers
  (is (equal (clean-phone "(703) 123-9876")
	     "7031239876"))
  (is (equal (pretty-print-phone "7031239876")
	     "(703) 123-9876"))
  )

(test sets
  (is (equal (make-set '(4 3 5 2 1 5 4 3 6 7 4 9 8 0 8 4 5 4 9 8))
	     '(0 1 2 3 4 5 6 7 8 9))))

(test timestamps
  (is (scan "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) [0-9][0-9]?, [0-9]{4} [0-9][0-9]?:[0-9]{2} (pm|am)"
	    (format-hr-timestamp (local-time:now)))))
