(in-package :cl-secure-auth)

(defclass user ()
  ((id :col-type :uuid
       :primary-key t
       :accessor user-id
       :initform (uuid:make-v4-uuid))
   (email :col-type :text
          :accessor user-email
          :unique t
          :not-null t)
   (password-hash :col-type :text
                  :not-null t
                  :accessor user-password-hash)
   (verified :col-type :boolean
             :initform nil
             :accessor user-verified)
   (roles :col-type  :jsonb
          :accessor user-roles-json
          :initform (cl-json:encode-json-alist-to-string
                     '(("roles" . ("user"))))))
  (:metaclass mito:dao-table-class)
  (:table-name "users"))


;; find user by ID. 
(defgeneric find-user-by-id (id)
  (:documentation "Find a user by their ID"))

(defmethod find-user-by-id ((id string))
  "Find user by their ID (if UUID is a string)"
  (mito:find-dao 'user :id id))

(defmethod find-user-by-id ((id uuid:uuid))
  "Find user by their ID (uuid object)"
  (mito:find-dao 'user :id id))


;; find user by email
(defgeneric find-user-by-email (email)
  (:documentation "Find a user by their email address"))

(defmethod find-user-by-email (email)
  (unless (validate-email email)
    (error 'user-error :message "Invalid email format"))
  (mito:find-dao 'user :email (string-downcase email)))

;; save user
(defgeneric save-user (user)
  (:documentation "Save or update a user"))

(defmethod save-user ((user user))
  (mito:save-dao user))

;; delete user
(defgeneric delete-user (user)
  (:documentation "Delete a user"))

(defmethod delete-user ((user user))
  (mito:delete-dao user))

;; create a user
(defun create-user (email password &key (verified nil))
  "Create a new user with the given email and password"
  (unless (validate-email email)
    ;; validate user email format
    (error 'user-error :message "Invalid email format"))
  (unless (validate-password password)
    ;; validate password format
    (error 'user-error :message "Password must be at least 8 characters and contain uppercase, lowercase, numbers and special characters"))
  ;; check if user exists
  (when (find-user-by-email email)
    (error 'user-error  :message "Email already registered"))
  ;; now we can register the user. First we hash the password
  (let* ((salt (cl-argon2:generate-salt))
         (password-hash (cl-argon2:argon2-hash-encoded password salt :type :argon2id)))
    (mito:create-dao 'user 
                     :email (string-downcase email)
                     :password-hash password-hash
                     :verified verified)))

(defun get-user-roles (user)
  "Get user roles as a list"
  (cdr (assoc :roles
              (cl-json:decode-json-from-string (user-roles-json user))
              :test #'equal)))

(defun set-user-roles (user role-list)
  "Set user roles from a list"
  (setf (user-roles-json user)
        (cl-json:encode-json-alist-to-string `(( "roles" . ,role-list))))
  (save-user user))


(defun add-user-role (user role)
  "Add a role to user if they don't already have it"
  (let ((current-roles (get-user-roles user)))
    (unless (member role current-roles :test #'string=)
      (set-user-roles user (cons role current-roles)))))

(defun remove-user-role (user role)
  "Remove a role from user if they have it"
  (let ((current-roles (get-user-roles user)))
    (when (member role current-roles :test #'string=)
      (set-user-roles user (remove role current-roles :test #'string=)))))

(defun user-has-role-p (user role)
  "Check if user has the specified role"
  (member role (get-user-roles user) :test #'string=))
