(in-package :cl-secure-auth)

(defparameter *session-prefix* "session:")
(defparameter *user-session-prefix* "user-sessions:")
(defparameter *blacklist-prefix* "blacklist:")


(defun create-session (user &key user-agent ip-address (duration *session-duration*))
  "Create a new session for a user"
  (let* ((session-id (princ-to-string (uuid:make-v4-uuid)))
         (claims `(("uuid" . ,(princ-to-string (user-id user)))
                   ("email" . ,(user-email user))
                   ("roles" . ,(get-user-roles user))
                   ("exp" . ,(+ (get-universal-time) duration))
                   ("iat" . ,(get-universal-time))
                   ("sid" . ,session-id)))
         (token (jose:encode :hs256 *signing-secret* claims))
         (session-data (cl-json:encode-json-alist-to-string
                        `(("user_id" . ,(princ-to-string (user-id user)))
                          ("token" . ,token)
                          ("user_agent" . ,user-agent)
                          ("ip_address" . ,ip-address)
                          ("created_at" . ,(get-universal-time))
                          ("last_active" . ,(get-universal-time))))))
    (redis:with-connection ()
      ;; store session data
      (redis:red-set (concatenate 'string *session-prefix* session-id) session-data)
      (redis:red-expire (concatenate 'string *session-prefix* session-id) duration)
      ;; add to user's session set
      (redis:red-sadd (concatenate 'string *user-session-prefix* (princ-to-string (user-id user)))))
    (values token session-id)))



;; Token blacklisting
(defun blacklist-token (token &optional claims)
  "Blacklist a token until its original expiration"
  (let* ((token-claims (or claims 
                           (verify-session-token token)))
         (expiry (cdr (assoc "exp" token-claims :test #'string=))))
    (when token-claims
      (redis:with-connection ()
        (redis:red-set (concatenate 'string *blacklist-prefix* token) "1")
        (when expiry
          (let ((ttl (max 0 (- expiry (get-universal-time)))))
            (redis:red-expire (concatenate 'string *blacklist-prefix* token) 
                              ttl)))))))

(defun token-blacklisted-p (token)
  "Check if a token is blacklisted"
  (redis:with-connection ()
    (redis:red-exists (concatenate 'string *blacklist-prefix* token))))


(defun get-session (session-id)
  "Get session data from Redis"
  (redis:with-connection ()
    (let ((session-json (redis:red-get
                         (concatenate 'string *session-prefix* session-id))))
      (when session-json
        (cl-json:decode-json-from-string session-json)))))




