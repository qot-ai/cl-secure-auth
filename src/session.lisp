(in-package :cl-secure-auth)

(defun blacklist-token (token &optional claims)
  "Blacklist a token until its original expiration"
  (let* ((token-claims (or claims 
                          (verify-session-token token)))
         (expiry (cdr (assoc "exp" token-claims :test #'string=))))
    (when token-claims
      (redis:with-connection ()
        (redis:red-set (format nil "blacklist:~A" token) "1")
        (when expiry
          (let ((ttl (max 0 (- expiry (get-universal-time)))))
            (redis:red-expire (format nil "blacklist:~A" token) ttl)))))))

(defun token-blacklisted-p (token)
  "Check if a token is blacklisted"
  (redis:with-connection ()
    (redis:red-exists (format nil "blacklist:~A" token))))

(defun logout (token)
  "Invalidate a session token"
  (let ((claims (verify-session-token token)))
    (blacklist-token token claims)
    t))
