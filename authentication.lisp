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
  (hash-function (concatenate '(simple-array (unsigned-byte 8) (*)) salt
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
  (equalp encrypted-password
	  (encrypt-password password salt)))

(defun create-account (name password &optional admin? address phone email description)
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
			   'admin admin?
			   'address address
			   'phone (when phone (clean-phone phone))
			   'email email
			   'description description))))

(defun lock-account (account-id)
  "Locks an account."
  (declare (type integer account-id))
  (execute (:update 'accounts :set 'locked? t
		    :where (:= 'account_id account-id))))

(defun unlock-account (account-id)
  "Unlocks an account."
  (declare (type integer account-id))
  (execute (:update 'accounts :set 'locked? nil
		    :where (:= 'account_id account-id))))

(defun remove-account (account-id)
  "Deletes an account."
  (declare (type integer account-id))
  (execute (:delete-from 'account :where (:= 'account_id account-id))))

(defun admin? (account-id)
  "Return true if account is an admin account."
  (declare (type integer account-id))
  (query (:select 'admin :from 'accounts :where (:= 'account_id account-id))
	 :single!))

(define-condition failed-authentication ()
  ((ip :initarg :ip :reader ip)
   (account-id :initarg :account-id :reader account-id)
   (message :initarg :message :reader message))
  (:report (lambda (condition stream)
	     (format stream "Failed Authentication: ~A~%source ip:  ~A~%account id: ~A"
		     (message condition)
		     (ip condition)
		     (account-id condition))))
  (:documentation "Condition signalling a failed login attempt."))

(define-condition nonexistant-account (failed-authentication) ())
(define-condition locked-account (failed-authentication) ())
(define-condition need-admin (failed-authentication) ())
(define-condition wrong-password (failed-authentication) ())

(defmacro authenticate ((account-id password ip &key need-admin?) &rest body)
  "Wraps a body of code and only executes it if account-id and password can be authenticated."
  (with-gensyms (normalized-ip)
    `(let ((,normalized-ip (normalize-ip ,ip)))
       (handler-case
	(unless ,account-id (error "Null account-id"))
	(unless ,password (error "Null password"))
	(unless ,ip (error "Null ip"))
	(destructuring-bind (encrypted-password password-salt admin? locked? last-login-attempt-time last-login-attempt-ip failed-logins)
	    (or (query (:select 'encrypted-password 'password-salt 'admin 'locked 'last-login-attempt-time 'last-login-attempt-ip 'failed-logins
				:from 'accounts
				:where (:= 'account_id ,account-id))
		       :row)
		(signal 'nonexistant-account :message "Account does not exist." :ip ,ip :account-id ,account-id))
	  ,(when need-admin?
	     `(when (not admin?)
		(signal 'need-admin "Need admin for this login." :ip ,ip :account-id ,account-id)))
	  (when locked?
	    (signal 'locked-account "Account locked." :ip ,ip :account-id ,account-id))
	  (sleep (greater (1- (expt 1.5 failed-logins)) 30 #'<))
	  (if (match-password ,password encrypted-password password-salt)
	      (progn (unless (zerop failed-logins) 
		       (execute (:update 'accounts :set 'failed_logins 0 :where (:= 'account_id ,account-id))))
		     (execute (:update 'accounts :set 'last_login_time (now) 'last_login_ip ,normalized-ip))
		     (progn ,@body))
	    (signal 'wrong-password "Password does not match." :ip ,ip :account-id ,account-id))))
       (wrong-password (c)
		       (execute (:update 'accounts :set 
					 'failed_logins (1+ failed-logins) 
					 'last_login_attempt_time (now) 
					 'last_login_attempt_ip ,normalized-ip
					 :where (:= 'account_id ,account-id)))
		       (write-to-string c))
       (failed-authentication (c)
			      (error c)))))

(defun has-permission? (account-id parish-id &rest permissions)
  "Returns true if the given account has permission to the perform the specified operation
   on the given parish/record"
  (declare (type integer account-id parish-id))
  (destructuring-bind (read write)
      (query (:select 'read 'write :from 'permissions 
		      :where (:and (:= 'account_id account-id) 
				   (:or (:is-null 'parish_id)
					(:= 'parish_id parish-id))))
	     :row)
    (let ((permissions (or permissions '(:read :write))))
      (and (or (not (member :read permissions)) read)
	   (or (not (member :write permissions)) write)))))
  
(defun grant-permission (account-id parish-id &rest permissions)
  "Grant a permissions on a parish to an account."
  (declare (type integer account-id)
	   (type (or integer keyword) parish-id))
  (let ((permissions (or permissions '(:read :write))))
    (execute (:insert-into 'permissions :set
			   'account_id account-id
			   'parish_id (if (equal parish-id :all) :null parish-id) ;null is the universal matcher for parish id in this table
			   'read (when (member :read permissions) t)
			   'write (when (member :write permissions) t)))))

(defun revoke-permission (account-id parish-id)
  "Revoke a permission on a parish for an account."
  (declare (type integer account-id)
	   (type (or integer keyword) parish-id))
  (execute (:delete-from 'permissions 
			 :where (:and (:= 'account_id account-id)
				      (:= 'parish_id (if (equal parish-id :all) :null parish-id))))))

		  
;;;TODO figure out better return codes
