(require 'http-post-simple)

(defun foo ()
  (interactive)
  (message "foop"))

(defun fullname-p (field)
  (unless t "Bad Fullname"))
(defun shortname-p (field)
  (unless t "Bad Shortname"))
(defun country-p (field)
  (unless (equal field "US") "Only 'US' supported currently"))
(defun state-p (field)
  (unless (member field '("AL" "AK" "AS" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FM" "FL" "GA"
			  "GU" "HI" "ID" "IL" "IN" "IA" "KS" "KY" "LA" "ME" "MH" "MD" "MA"
			  "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ" "NM" "NY" "NC" "ND"
			  "MP" "OH" "OK" "OR" "PW" "PA" "PR" "RI" "SC" "SD" "TN" "TX" "UT"
			  "VT" "VI" "VA" "WA" "WV" "WI" "WY"))
    "Must be standard two-letter state abbreviation"))
(defun city-p (field)
  (unless t "Bad city name"))
(defun street-p (field)
  (unless t "Bad street name"))
(defun street-number-p (field)
  (unless (integerp (car (read-from-string field)))
    "Bad street number: not a number"))
(defun zip-p (field)
  (unless (integerp (car (read-from-string field)))
    "Bad zip code: not a number"))
(defun phone-p (field)
  (unless t "Bad phone number"))
(defun email-p (field)
  (unless (or (equal "" field) (string-match ".*@.*\..*" field))
    "Bad email address"))
(defun website-p (field)
  (unless t "Bad Website"))
(defun coordinate-p (field)
  (unless (floatp (car (read-from-string field)))
    "Bad coordinates"))
(defun diocese-p (field)
  (unless t "Bad diocese name"))


(defvar parish-template
  (list #'fullname-p #'shortname-p #'country-p #'state-p #'city-p #'street-p #'street-number-p
	#'zip-p #'phone-p #'email-p #'website-p #'coordinate-p #'coordinate-p #'diocese-p))

; LocalWords:  CSV csv LocalWords Keymap
(define-derived-mode sacraspot-mode csv-validate-mode "Sacraspot"
  "A major mode for working with sacraspot"
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map csv-validate-mode-map)
    (define-key map (kbd "C-c l") 'foo)
    (use-local-map map)))

(provide 'sacraspot-mode)

(defun submit-parish ()
  (interactive)
  (print (http-post-simple "http://www.beggersandbuskers.com:8080/insert-parishes"
			   `((parishes . ,(buffer-string))))))

(defun submit-schedule ()
  (interactive)
  (print (http-post-simple "http://www.beggersandbuskers.com:8080/insert-schedules"
			   `((schedules . ,(buffer-string))))))


(defun format-schedule ()
  (interactive)
  (thing-at-point 'line)
