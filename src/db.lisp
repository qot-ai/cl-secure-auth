(in-package :cl-secure-auth)


(defclass user-table ()
  ((id :col-type :text
       :primary-key t
       :accessor user-id)
   (email :col-type :text
          :unique t
          :not-null t
          :accessor user-email)
   (password-hash :col-type :text
                  :not-null t
                  :accessor user-password-hash)
   (verified :col-type :boolean
             :initform nil
             :accessor user-verified)
   (roles :col-type  :jsonb
          :initform (list "user")
          :accessor :user-roles))
  (:metaclass mito:dao-table-class)
  (:table-name "users"))

(defclass reset-token ()
  ((token :col-type :text
          :primary-key t)
   (user :col-type user-table
         :foreign-key t)
   (expires-at :col-type :timestamp
               :not-null t))
  (:metaclass mito:dao-table-class)
  (:table-name "reset_tokens"))


(defclass verification-token ()
  ((token :col-type :text
          :primary-key t)
   (user :col-type user-table
         :foreign-key t)
   (expires-at :col-type :timestamp
               :not-null t))
  (:metaclass mito:dao-table-class)
  (:table-name "verification_tokens"))


;; database connection management

(defun init-db (&key database username password (host "localhost") (port 5432))
  "Initialize database connection with Mito"
  (mito:connect-toplevel :postgres
                         :database-name database
                         :username username
                         :password password
                         :host host
                         :port port))


(defun ensure-tables ()
  "Ensure all tables exist and are up-to-date"
  (mapc #'mito:ensure-table-exists '(user-table reset-token verification-token)))


(defmethod save-user ((user user))
  "Save or update a user in the database"
  (let ((db-user (or (find-user-by-id (user-id user))
                     (make-instance 'user-table))))
    (setf (user-id db-user) (user-id user)
          (user-email db-user) (user-email user)
          (user-password-hash db-user) (user-password-hash user)
          (user-verified db-user) (user-verified user)
          (user-roles db-user) (user-roles user))
    (mito:save-dao db-user)
    user))

(defmethod find-user-by-id ((id string))
  "Find a user by their ID"
  (let ((db-user (mito:find-dao 'user-table :id id)))
    (when db-user
      (make-instance 'user
                     :id (user-id db-user)
                     :email (user-email db-user)
                     :password-hash (user-password-hash db-user)
                     :verified (user-verified db-user)
                     :roles (user-roles db-user)))))

(defmethod find-user-by-email ((email string))
  "Find a user by their email address"
  (let ((db-user (mito:find-dao 'user-table :email email)))
    (when db-user
      (make-instance 'user
                     :id (user-id db-user)
                     :email (user-email db-user)
                     :password-hash (user-password-hash db-user)
                     :verified (user-verified db-user)
                     :roles (user-roles db-user)))))


(defmethod delete-user ((user user))
  "Delete a user from the database"
  (let ((db-user (mito:find-dao 'user-table :id (user-id user))))
    (when db-user
      (mito:delete-dao db-user))))



(defun save-reset-token (token user-id expires-at)
  "Save a password reset token"
  (let ((user (mito:find-dao 'user-table :id user-id)))
    (when user
      (mito:create-dao 'reset-token
                       :token token
                       :user user
                       :expires-at expires-at))))

(defun save-verification-token (token user-id expires-at)
  "Save an email verification token"
  (let ((user (mito:find-dao 'user-table :id user-id)))
    (when user
      (mito:create-dao 'verification-token
                       :token token
                       :user user
                       :expires-at expires-at))))

(defun find-valid-reset-token (token)
  "Find a valid (non-expired) password reset token"
  (mito:find-dao 'reset-token
                 :token token
                 :where (:> :expires-at (local-time:now))))


(defun find-valid-verification-token (token)
  "Find a valid (non-expired) verification token"
  (mito:find-dao 'verification-token
                 :token token
                 :where (:> :expires-at (local-time:now))))

(defun migrate-datase ()
  "Run any pending database migrations"
  (mito:migrate-table 'user-table)
  (mito:migrate-table 'reset-token)
  (mito:migrate-table 'verification-token))



