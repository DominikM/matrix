(asdf:defsystem :matrix
  :description ""
  :version "0.1"
  :author "Dominik Martinez"
  :depends-on (:woo
	       :clack
	       :com.inuoe.jzon
	       :clsql-postgresql-socket3
	       :alexandria
	       :uuid)
  :components ((:file "matrix")))
