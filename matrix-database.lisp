(defpackage :matrix-database
  (:use :cl)
  (:export #:create
	   #:drop
	   #:reset
	   #:initialize
	   
	   #:get-user
	   #:create-user
	   #:authenticate-user

	   #:get-device
	   #:create-device)
  (:import-from :clsql #:with-database))

(in-package :matrix-database)

(clsql:locally-enable-sql-reader-syntax)

(defvar +database-spec+
  '("localhost"     ; hostname
    "matrix_server" ; database name
    "postgres"      ; username
    ""))            ; password

(defun create ()
  (clsql:create-database +database-spec+))

(defun drop ()
  (clsql:destroy-database +database-spec+))

(defun reset ()
  (clsql:disconnect-pooled)
  (drop)
  (create)
  (initialize))

(defun initialize ()
  (with-database (db +database-spec+ :pool t)
    (clsql:create-table 'users
			'((username "varchar(100)" :primary-key)
			  (password "text" :not-null))
			:database db)
    (clsql:create-table 'devices
			'((id "text" :primary-key)
			  (display-name "text")
			  (username "varchar(100)" :references users)
			  (access_token "text")
			  (refresh_token "text"))
			:database db)))

(defun get-user (username)
  (let* ((result (with-database (db +database-spec+ :pool t)
		   (clsql:select '*
				 :from 'users
				 :where [= [username] username]
				 :database db)))
	 (user (car result)))
    (when user
      (list :username (nth 0 user)))))

(defun create-user (username password)
  (let ((hashed-password (cl-pass:hash password)))
    (with-database (db +database-spec+ :pool t)
      (clsql:insert-records :into 'users
			    :attributes '(username password)
			    :values (list username hashed-password)
			    :database db))))

(defun authenticate-user? (username password)
  (with-database (db +database-spec+ :pool t)
    (let* ((hashed-password (clsql:select 'password :from 'users :database db :where [= [username] username])))
	   (cl-pass:check-password password hashed-password))))

(defun get-device (username device-id)
  (let* ((result (with-database (db +database-spec+ :pool t)
		  (clsql:select '*
				:from 'devices
				:where [and [= [username] username] [= [id] device-id]]
				:database db)))
	(device (car result)))
    (list :id            (nth 0 device)
	  :display-name  (nth 1 device)
	  :username      (nth 2 device)
	  :access-token  (nth 3 device)
	  :refresh-token (nth 4 device))))

(defun create-device (username device-id display-name)
  (let ((access-token (write-to-string (uuid:make-v4-uuid)))
	(refresh-token (write-to-string (uuid:make-v4-uuid))))
    (with-database (db +database-spec+ :pool t)
      (clsql:insert-records :into 'devices
			    :attributes '(id display-name username access_token refresh_token)
			    :values (list device-id display-name username access-token refresh-token)
			    :database db))
    (list :id device-id
	  :display-name display-name
	  :username username
	  :access-token access-token
	  :refresh-token refresh-token)))
    
(clsql:restore-sql-reader-syntax-state) 
