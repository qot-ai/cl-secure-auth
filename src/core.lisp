(in-package :cl-secure-auth)


;; Configuration
(defparameter *signing-secret* nil)
(defparameter *session-duration* (* 24 60 60)) ; 24 hours in seconds
(defparameter *redis-conn* nil)

;; initialization



(defun initialize-auth (&key signing-secret redis-host redis-port)
  "Initialize the authentication system"
  ;; Always update signing secret
  (setf *signing-secret* (ironclad:ascii-string-to-byte-array signing-secret))
  ;; Try to connect to Redis only if not already connected
  (handler-case 
      (progn
        ;; Only attempt to connect if we're not already connected
        (unless (redis:connected-p)
          (setf *redis-conn* (redis:connect :host redis-host :port redis-port)))
        t)  ; Return true for successful initialization
    (error (e)
      (error "Failed to connect to Redis: ~A" e))))


;; Token generation and verification
(defun generate-session-token (user-id roles &key(duration *session-duration*))
  "Create a new session token using JOSE"
  (let* ((exp (+ (get-universal-time) duration))
         (claims `(("uuid" . ,user-id)
                   ("roles" . ,roles)
                   ("exp" . ,exp)
                   ("iat" . ,(get-universal-time)))))
    (jose:encode :hs256 *signing-secret* claims)))


(defun verify-session-token (token)
  "Verify and decode a session token"
  ;; First check for basic token validity
  (when (or (null token)
            (string= token "")
            (< (count #\. token) 2))
    (error 'invalid-token-error))

  ;; Try to decode and verify the token
  (let ((claims (handler-case
                   (jose:decode :hs256 *signing-secret* token)
                 (error ()
                   (error 'invalid-token-error)))))
    ;; Token was decoded, now check validity conditions
    (cond
      ;; Null claims means invalid token
      ((null claims)
       (error 'invalid-token-error))
      ;; Check if token has been blacklisted
      ((token-blacklisted-p token)
       (error 'blacklisted-token-error))
      ;; Check expiration
      ((let ((exp (cdr (assoc "exp" claims :test #'string=))))
         (and exp (> (get-universal-time) exp)))
       (error 'session-expired-error))
      ;; If all checks pass, return the claims
      (t claims))))


(defmacro with-auth ((token &key required-roles) &body body)
  "Execute body with authentication and optional role verification"
  `(let ((claims (verify-session-token ,token)))
     (when ,required-roles
       (let ((user-roles (cdr (assoc "roles" claims :test #'string=))))
         (unless (intersection ,required-roles user-roles :test #'string=)
           (error 'permission-denied-error))))
     (let ((*current-user-id* (cdr (assoc "uid" claims :test #'string=)))
           (*current-user-roles* (cdr (assoc "roles" claims :test #'string=))))
       ,@body)))

(defun has-role-p (claims role)
  "Check if the claims contain a specific role"
  (let ((roles (cdr (assoc "roles" claims :test #'string=))))
    (member role roles :test #'string=)))
