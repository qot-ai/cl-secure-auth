(in-package :cl-secure-auth)

;; utilities

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




