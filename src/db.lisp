(in-package :cl-secure-auth)

;; Database connection management
(defun init-db (&key database username password (host "localhost") (port 5432))
  "Initialize database connection with Mito"
  (mito:connect-toplevel :postgres
                        :database-name database
                        :username username
                        :password password
                        :host host
                        :port port))

(defparameter *dao-classes*
  ;; Add all DAO classes here
  '(user))


(defun ensure-tables ()
  "Ensure all tables exist and are up to date"
  (mapc #'mito:ensure-table-exists  *dao-classes* ))


(defun migrate-database ()
  "Run any pending migrations"
  (mapc #'mito:migrate-table *dao-classes*)
  )
