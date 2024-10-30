(in-package :cl-secure-auth)

(defclass user ()
  ((id :initarg :id
       :accessor user-id
       :type string
       :documentation "Unique identifier for the user")
   (email :initarg :email
         :accessor user-email
         :type string
         :documentation "User's email address")
   (password-hash :initarg :password-hash
                  :accessor user-password-hash
                  :type string
                  :documentation "Argon2 hashed password")
   (verified :initarg :verified
             :accessor :user-verified
             :initform nil
             :type boolean
             :documentation "Whether the user email is verified")
   (created-at :initarg :created-at
               :accessor user-created-at
               :initform (local-time:now)
               :type local-time:timestamp
               :documentation "When the user account was created")
   (roles  :initarg :roles
           :accessor user-roles
           :initform '("user")
           :type list
           :documentation "List of roles assigned to the user")))


(defgeneric find-user-by-id (id)
  (:documentation "Find a user by their ID"))

(defgeneric find-user-by-email (email)
  (:documentation "Find a user by their email address"))

(defgeneric save-user (user)
  (:documentation "Save or update a user"))

(defgeneric delete-user (user)
  (:documentation "Delete a user"))


(define-condition user-error (error)
  ((message :initarg :message :reader user-error-message))
  (:report (lambda (condition stream)
             (format stream "User error: ~A" (user-error-message condition)))))

(defun validate-email (email)
  "Validate email using a simple regex"
  (cl-ppcre:scan "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" email))

(defun validate-password (password)
  "Validate password strength requiring uppercase, lowercase, numbers, and special characters"
  (and (>= (length password) 8)
       (cl-ppcre:scan "[A-Z]" password)      ; uppercase
       (cl-ppcre:scan "[a-z]" password)      ; lowercase
       (cl-ppcre:scan "[0-9]" password)      ; numbers
       (cl-ppcre:scan "[!@#$%^&*(),.?\":{}|<>]" password))) ; special characters




