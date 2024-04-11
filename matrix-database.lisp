(defpackage :matrix-database
  (:use :cl)
  (:export #:create #:drop #:reset #:initialize #:get-user))

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
  (clsql:with-database (db +database-spec+ :pool t)
    (clsql:create-table 'users
			'((username "varchar(100)" :not-null :unique :primary-key))
			:database db)))

(defun get-user (username)
  (clsql:with-database (db +database-spec+ :pool t)
    (clsql:select '*
		  :from 'users
		  :database db
		  :where [= [username] username])))
