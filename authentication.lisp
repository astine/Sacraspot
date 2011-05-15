(cl:in-package #:sacraspot)

;;; Creating the pseudo-random-sequence

(defun make-random-number (limit) ;TODO replace inbuilt 'random' with a cryptographically secure version
  "Returns a random number."
  (random limit))

(defun test-prime (prime-candidate)
  "Returns true if 'prime-candidate' is a prime number."
  (when (zerop (mod prime-candidate 2))
    (return-from test-prime nil))
  (when (zerop (mod prime-candidate 3))
    (return-from test-prime nil))
  (loop for factor1 from 5 to (1+ (round (sqrt prime-candidate))) by 6
	for factor2 from 7 to (1+ (round (sqrt prime-candidate))) by 6
	when (or (zerop (mod prime-candidate factor1))
		 (zerop (mod prime-candidate factor2)))
	do (return-from test-prime nil))
  t)

(defvar *current-time-factor* 100000 
  "Current complexity factor for password hashing.")

(defun hash-function (byte-array time-factor)
  "Hashes a given 'byte-array' using the sha512 digest operation.
   'time-factor' specifies repeat operations to increase the computational
   complexity of the hash."
  (when (< time-factor 1)
    (error "Time factor must be at least 1"))
  (let ((digester (make-digest :sha512)))
    (loop repeat time-factor
	  for digest = (digest-sequence digester byte-array)
	  then (progn (reinitialize-instance digester)
		      (digest-sequence digester digest))
	  finally (return digest))))

(defun encrypt-password (password salt)
  "Hashes a password with a given salt"
  (hash-function (concatenate 'simple-array salt
			      (ascii-string-to-byte-array password))
		 *current-time-factor*))

(defun generate-salt (length)
  "Generates a random byte array of 'length' length for use as a password
   salt."
  (map `(simple-array (unsigned-byte 8) (,length)) 
       (lambda (_) (make-random-number 256))
       (make-array length :element-type '(unsigned-byte 8))))
  

(defun match-password (password encrypted-password salt)
  "Returns true if 'encrypted-password' is the hash of 'salt' + 'password'"
  (equal encrypted-password
	 (encrypt-password password salt)))

(defun create-account (name password &optional address phone email description)
  "Creates a new account and inserts it into the database."
  ;;validate info
  (unless (or (null email)
	      (equal "" email)
	      (scan "[a-zA-Z0-9_.-]+@[a-zA-Z0-9_.-]+[.].+" email))
    (error "Bad email address being added to parish table"))
  (unless (or (null phone)
	      (equal "" phone)
	      (= (length (clean-phone phone)) 10))
    (error "Bad phone number being added to parish table"))
  (when (or (null password)
	    (< (length password) 8))
    (error "Improper password"))
  (let* ((password-salt (generate-salt 16))
	 (encrypted-password (encrypt-password password password-salt)))
    (execute (:insert-into 'accounts :set
			   'password_salt password-salt
			   'encrypted_password encrypted-password
			   'name name
			   'address address
			   'phone (clean-phone phone)
			   'email email
			   'description description))))

(defmacro authenticate ((account-id password ip) &rest body)
  "Wraps a body of code and only executes it if account-id and password can be authenticated."
  `(catch 'break-from-authentication
     (destructuring-bind (encrypted-password password-salt locked? last-login-attempt-time last-login-attempt-ip failed-logins)
	 (query (:select 'encrypted-password 'password-salt 'locked? 'last-login-attempt-time 'last-login-attempt-ip 'failed-logins
			 :from 'accounts
			 :where (:= 'account_id ,account-id))
		:row)
       (when locked?
	 (warn "Attempt to access locked account ~A from ip ~A" ,account-if ,ip)
	 (throw 'break-from-authentication "Account Locked"))
       (sleep (greater (1- (expt 1.5 failed-logins)) 30 #'<))
       (if (match-password ,password encrypted-password password-salt)
	   (progn (unless (zerop failed-logins) 
		    (execute (:update 'accounts :set 'failed_logins 0 :where (:= 'account_id ,account-id))))
		  (execute (:update 'accounts :set 'last_login_time (now) 'last_login_ip ,ip))
		  (progn ,@body))
	 (execute (:update 'accounts :set 
			   'failed_logins (1+ failed-logins) 
			   'last_login_attempt_time (now) 
			   'last_login_attempt_ip ,ip
			   :where (:= 'account_id ,account-id)))
	 (warn "Failed authentication from: ~A using id: ~A and password: ~A"
	       ,ip ,account-id ,password)
	 "Failed login"))))
			   
		  
;;;TODO figure out better return codes
