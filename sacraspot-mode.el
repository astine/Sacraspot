(require 'http-post-simple)
(require 'json)

(defun read-string (string)
  (ignore-errors
    (read-from-string string)))

(defun ensure-string (item)
  "Converts item to a string representation if applicable"
  (typecase item
    (integer (format "%i" item))
    (null "")
    (symbol (format "%s" item))
    (string item)))

(defvar *months* '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(defvar *days-of-week* '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defvar *states* '("AL" "AK" "AS" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FM" "FL" "GA"
		   "GU" "HI" "ID" "IL" "IN" "IA" "KS" "KY" "LA" "ME" "MH" "MD" "MA"
		   "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ" "NM" "NY" "NC" "ND"
		   "MP" "OH" "OK" "OR" "PW" "PA" "PR" "RI" "SC" "SD" "TN" "TX" "UT"
		   "VT" "VI" "VA" "WA" "WV" "WI" "WY"))

;;parish template
(defun fullname-p (field)
  (unless t "Bad Fullname"))
(defun shortname-p (field)
  (unless t "Bad Shortname"))
(defun country-p (field)
  (unless (equal field "US") "Only 'US' supported currently"))
(defun state-p (field)
  (unless (member field *states*)
    "Must be standard two-letter state abbreviation"))
(defun city-p (field)
  (unless t "Bad city name"))
(defun street-p (field)
  (unless t "Bad street name"))
(defun street-number-p (field)
  (unless (or (equal field "")
	      (every (lambda (char) (and (< 47 char) (< char 58))) field))
    "Bad street number: not a number"))
(defun zip-p (field)
  (unless (integerp (car (read-string field)))
    "Bad zip code: not a number"))
(defun phone-p (field)
  (unless t "Bad phone number"))
(defun email-p (field)
  (unless (or (equal field "")
	      (string-match ".*@.*\..*" field))
    "Bad email address"))
(defun website-p (field)
  (unless t "Bad Website"))
(defun coordinate-p (field)
  (unless (floatp (car (read-string field)))
    "Bad coordinates"))
(defun diocese-p (field)
  (unless t "Bad diocese name"))


(defvar parish-template
  (list #'fullname-p #'shortname-p #'country-p #'state-p #'city-p #'street-p #'street-number-p
	#'zip-p #'phone-p #'email-p #'website-p #'coordinate-p #'coordinate-p #'diocese-p))

;; schedule template
(defun parish-id-p (field)
  (unless (integerp (car (read-string field)))
    ("Bad ID: not an integer")))
(defun sacrament-type-p (field)
  (unless (member field '("Mass","Confession","Adoration"))
    "Invalid sacrament type: Must be Mass, Confession, or Adoration"))
(defun time-p (field)
  (unless (string-match "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\s[PA]M" field)
    "Bad format for time: Should be hh:mm:ss AM/PM"))
(defun description-p (field)
  (unless t "Bad descriptions"))
(defun year-list-p (field)
  (unless (or (equal field "")
	      (aand (car (read-string field))
		    (and (listp it) (every #'integerp it))))
    "Bad year list"))
(defun month-list-p (field)
  (unless (or (equal field "")
	      (aand (car (read-string field))
		    (and (listp it) (every (lambda (item) (member item *months*)) it))))
    "Bad month list"))
(defun dom-list-p (field)
  (unless (or (equal field "")
	      (aand (car (read-string field))
		    (and (listp it) (every #'integerp it))))
    "Bad day of month list"))
(defun dow-list-p (field)
  (let ((foo (car (read-string field))))
  (unless (or (equal field "")
	      (aand (car (read-string field))
		    (and (listp it) (every (lambda (item) (member item *days-of-week*)) it))))
    "Bad day of month list" (every (lambda (item) (member item *days-of-week*)) foo))))

(defvar schedule-template
  (list #'parish-id-p #'sacrament-type-p #'time-p #'time-p #'description-p
	#'year-list-p #'month-list-p #'dom-list-p #'dow-list-p))

(defvar parishes/schedules :parishes)

(defun submit-parish (start end)
  (http-post-simple "http://www.beggersandbuskers.com:8080/insert-parishes"
		    `((parishes . ,(buffer-substring start end)))))

(defun submit-schedule (start end)
  (http-post-simple "http://www.beggersandbuskers.com:8080/insert-schedules"
		    `((schedules . ,(buffer-substring start end)))))

(defun submit ()
  (aif (validate-csv-buffer start end)
       (concat "Cannot submit because of errors:\n"
	       (print-errors it))
       (case parishes/schedules
	 (:parishes (submit-parish start end))
	 (:schedules (submit-schedule start end)))))

(defun submit-buffer ()
  (interactive)
  (message (submit (point-min) (point-max))))

(defun submit-region (start end)
  (interactive "r")
  (message (submit start end)))
 
(defun json-to-csv (json)
  (mapconcat (lambda (row)
	       (mapconcat (lambda (element)
			    (format "\"%s\"" (ensure-string (cdr element))))
			  (nreverse row)
			  ","))
	     (map 'list #'identity (json-read-from-string json))
	     "\n"))
    
(defun json-row-to-csv (json-row)
  (mapconcat #'cdr json-row ","))

(defun* query-parish (&optional parish-id fullname shortname
			   country state city street street-number zip
			   phone email website
			   latitude longitude diocese)
  (json-to-csv
   (car
    (http-post-simple "http://www.beggersandbuskers.com:8080/select-parishes"
		      (delq nil
			    (list
			     (when parish-id `(parish-id . ,(ensure-string parish-id)))
			     (when fullname `(fullname . ,(ensure-string fullname)))
			     (when shortname `(shortname . ,(ensure-string shortname)))
			     (when country `(country . ,(ensure-string country)))
			     (when state `(state . ,(ensure-string state)))
			     (when city `(city . ,(ensure-string city)))
			     (when street `(street . ,(ensure-string street)))
			     (when street-number `(street-number . ,(ensure-string street-number)))
			     (when zip `(zip . ,(ensure-string zip)))
			     (when phone `(phone . ,(ensure-string phone)))
			     (when email `(email . ,(ensure-string email)))
			     (when website `(website . ,(ensure-string website)))
			     (when latitude `(latitude . ,(ensure-string latitude)))
			     (when longitude `(longitude . ,(ensure-string longitude)))
			     (when diocese `(diocese . ,(ensure-string diocese)))))))))

(defun select-parish (&rest args)
  (interactive "MParish-ID: \nMFullname: \nMShortname: \nMCountry: \nMState: \nMCity: \nMStreet: \nMStreet-Number: \nMZip: \nMPhone: \nMEmail: \nMWebsite: \nMLatitude: \nMLongitude: \nMDiocese")
  (with-output-to-temp-buffer "*parishes*"
    (print (apply #'query-parish args))))
  

(defun toggle-parishes/schedules ()
  "Switches between parishes and schedules modes"
  (interactive)
  (case parishes/schedules
    (:parishes (setq parishes/schedules :schedules)
	       (setq *template* schedule-template)
	       (message "Switched from parishes to schedules"))
    (:schedules (setq parishes/schedules :parishes)
		(setq *template* parish-template)
		(message "Switched from schedules to parishes"))))

;(defun format-schedule ()
  ;(interactive)
  ;(thing-at-point 'line)

(define-derived-mode sacraspot-mode csv-validate-mode "Sacraspot"
  "A major mode for working with sacraspot"
  (setq *template* parish-template)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map csv-validate-mode-map)
    (define-key map (kbd "C-c s") 'submit-region)
    (define-key map (kbd "C-c S") 'submit-buffer)
    (define-key map (kbd "C-c t") 'toggle-parishes/schedules)
    (use-local-map map)))

(provide 'sacraspot-mode)
