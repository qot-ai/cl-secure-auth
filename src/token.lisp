(in-package :cl-secure-auth)

(defclass reset-token ()
  ((token :col-type :text
          :primary-key t
          :accessor token)
   (token-hash :col-type :text  
               :not-null t
               :accessor token-hash)
   (user-id :col-type :text
            :not-null t
            :accessor user-id)
   (expires-at :col-type :timestamp
               :not-null t
               :accessor expires-at))
  (:metaclass mito:dao-table-class)
  (:table-name "reset_tokens"))

(defun create-reset-token (user-id &key (expires-in (* 24 60 60)))
  "Create a new reset token for a user"
  (let* ((raw-token (ironclad:byte-array-to-hex-string 
                     (ironclad:random-data 32))) ; 32 bytes of random data
         (salt (cl-argon2:generate-salt))
         (token-hash (cl-argon2:argon2-hash-encoded 
                     raw-token salt :type :argon2id)))
    ;; Save token to database
    (mito:create-dao 'reset-token
                     :token raw-token
                     :token-hash token-hash
                     :user-id user-id
                     :expires-at (local-time:timestamp+ 
                                (local-time:now) expires-in :sec))
    ;; Return the raw token to be sent to user
    raw-token))

;; Enhanced find-valid-reset-token function
(defun find-valid-reset-token (raw-token)
  "Find and verify a valid (non-expired) reset token"
  (let ((token-record (mito:find-dao 'reset-token
                                    :token raw-token
                                    :where (:> :expires-at (local-time:now)))))
    (when (and token-record
               (cl-argon2:verify-password 
                (token-hash token-record) 
                raw-token 
                :type :argon2id))
      token-record)))

