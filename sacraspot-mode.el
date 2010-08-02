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

(defvar *server-base-url* "http://www.beggersandbuskers.com:8080/")

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

;;; Querying

(defun json-to-lists (json)
  "Converts a json string to a list of lists"
  (map 'list #'identity
       (json-read-from-string json)))
 
(defun to-csv (server-output)
  "Converts a list of association lists to csv rows
   Used for printing data got from the server"
  (mapconcat (lambda (row)
	       (mapconcat (lambda (element)
			    (format "\"%s\"" (ensure-string (cdr element))))
			  (nreverse row)
			  ","))
	     server-output
	     "\n"))

(defun* query-parish (&optional parish-id fullname shortname
				country state city street street-number zip
				phone email website
				latitude longitude diocese)
  "Queries the server for parishes and returns them as a list of association lists."
  (json-to-lists
   (car
    (http-post-simple (concat *server-base-url* "select-parishes")
		      (delq nil
			    (list
			     (when parish-id `(parish-id . ,(prin1-to-string parish-id)))
			     (when fullname `(fullname . ,(prin1-to-string fullname)))
			     (when shortname `(shortname . ,(prin1-to-string shortname)))
			     (when country `(country . ,(prin1-to-string country)))
			     (when state `(state . ,(prin1-to-string state)))
			     (when city `(city . ,(prin1-to-string city)))
			     (when street `(street . ,(prin1-to-string street)))
			     (when street-number `(street-number . ,(prin1-to-string street-number)))
			     (when zip `(zip . ,(prin1-to-string zip)))
			     (when phone `(phone . ,(prin1-to-string phone)))
			     (when email `(email . ,(prin1-to-string email)))
			     (when website `(website . ,(prin1-to-string website)))
			     (when latitude `(latitude . ,(prin1-to-string latitude)))
			     (when longitude `(longitude . ,(prin1-to-string longitude)))
			     (when diocese `(diocese . ,(prin1-to-string diocese)))))))))
  ))

(defun* select-parish (&rest args)
  "Queries the server for parishes and shows them in a separate buffer"
  (interactive "MParish-ID: \nMFullname: \nMShortname: \nMCountry: \nMState: \nMCity: \nMStreet: \nMStreet-Number: \nMZip: \nMPhone: \nMEmail: \nMWebsite: \nMLatitude: \nMLongitude: \nMDiocese: ")
  (with-output-to-temp-buffer "*parishes*"
    (print (to-csv (apply #'query-parish args)))))

(defun insert-select-parish (&rest args)
  "Queries the server for parishes and inserts them at the point"
  (interactive "MParish-ID: \nMFullname: \nMShortname: \nMCountry: \nMState: \nMCity: \nMStreet: \nMStreet-Number: \nMZip: \nMPhone: \nMEmail: \nMWebsite: \nMLatitude: \nMLongitude: \nMDiocese: ")
  (insert (to-csv (apply #'query-parish args))))

;;; Submission

(defun submit-parish (start end)
  "Submit region as parishes"
  (concat
   "IDs Assigned: "
   (mapconcat #'prin1-to-string 
	      (json-to-lists
	       (car (http-post-simple (concat *server-base-url* "insert-parishes")
				      `((parishes . ,(buffer-substring-no-properties start end))))))
	      ", ")))
   

(defun submit-schedule (start end)
  "Submit region as schedules"
  (http-post-simple (concat *server-base-url* "insert-schedules")
		    `((schedules . ,(buffer-substring-no-properties start end)))))

(defun submit (start end)
  "Submits a number of rows to the database
   these rows are first validated and then the proper submission function
   is called depending on whether parishes or schedules are active."
  (aif (validate-csv-region start end)
       (concat "Cannot submit because of errors:\n"
	       (print-errors it))
       (case parishes/schedules
	 (:parishes (submit-parish start end))
	 (:schedules (submit-schedule start end)))))

(defun submit-buffer ()
  "Submits the entire buffer"
  (interactive)
  (message (submit (point-min) (point-max))))

(defun submit-region (start end)
  "Submits the region between the mark and the point"
  (interactive "r")
  (message (submit start end)))

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
