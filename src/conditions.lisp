(in-package :cl-secure-auth)

(define-condition auth-error (error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (c s) 
             (format s "Authentication error: ~A" (error-message c)))))

(define-condition invalid-token-error (auth-error) ()
  (:default-initargs :message "Invalid token"))

(define-condition session-expired-error (auth-error) ()
  (:default-initargs :message "Session expired"))

(define-condition blacklisted-token-error (auth-error) ()
  (:default-initargs :message "Token has been revoked"))

(define-condition permission-denied-error (auth-error) ()
  (:default-initargs :message "Permission denied"))

(define-condition user-error (error)
  ((message :initarg :message :reader user-error-message))
  (:report (lambda (condition stream)
             (format stream "User error: ~A" (user-error-message condition)))))
