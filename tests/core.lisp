(defpackage :cl-secure-auth/tests/core
  (:use :cl :rove)
  (:import-from :cl-secure-auth
   :initialize-auth
   :generate-session-token
   :verify-session-token
   :with-auth
   :has-role-p
   :token-blacklisted-p
   :blacklist-token
   :logout
   :store-session-data
   :get-session-data
   :delete-session-data
   :invalid-token-error
   :session-expired-error
   :permission-denied-error
   :blacklisted-token-error
   ))

(in-package :cl-secure-auth/tests/core)

(defun setup-test-env ()
  (initialize-auth
   :signing-secret "test-secret"
   :redis-host "localhost"
   :redis-port 6379))

(defun ensure-redis-connection ()
  "Ensure Redis is connected, reconnect if necessary"
  (unless (redis:connected-p)  
    (setup-test-env)))

(defun cleanup-test-data ()
  "Clean up test data while preserving connection"
  (when (redis:connected-p)  
    (redis:with-connection ()
      (redis:red-flushdb))))


(defmacro with-clean-redis (&body body)
 `(progn
    (ensure-redis-connection)
    (cleanup-test-data)
    ,@body))



(deftest initialization-test
  (testing "auth system initialization"
    (when (redis:connected-p)
      (redis:disconnect))
    ;; test first initialization
    (ok (setup-test-env) "should initialize successfully first time")
    ;; test repeated initialization
    (ok (setup-test-env) "should handle repeated initialization ")
    ;; test initialization with different secret but same connection
    (ok (initialize-auth
         :signing-secret "different-secret"
         :redis-host "localhost"
         :redis-port 6379)
        "should update secret while maintaining connection")))

(deftest token-creation-test
  (testing "token generation"
    (with-clean-redis
      (let* ((user-id "test-user")
             (roles '("user" "admin"))
             (token (generate-session-token user-id roles)))
        (ok token "Should generate token")
        (ok (stringp token) "Token should be a string")
        (ok (> (length token) 0) "Token should not be empty")
        (ok (= 2 (count #\. token)) "Token should have three segments"))
      )))


(deftest token-verification-test
  (testing "token verification"
    (with-clean-redis
      (let* ((user-id "test-user")
             (roles '("user" "admin"))
             (token (generate-session-token user-id roles))
             (claims (verify-session-token token)))

        (ok claims "should verify and return claims")
        (ok (equal (cdr (assoc "uuid" claims :test #'string=)) user-id)
            "should have correct user ID in claims")
        (ok (equal (cdr (assoc "roles" claims :test #'string=)) roles)
            "should have correct roles in claims")
        (ok (numberp (cdr (assoc "exp" claims :test #'string=)))
            "should have expiration timestamp")
        (ok (numberp (cdr (assoc "iat" claims :test #'string=)))
            "should have issued-at timestamp")))))

(deftest token-validation-errors-test
  (testing "Initialization"
    (ok (initialize-auth :signing-secret "test-secret"
                         :redis-host "localhost"
                         :redis-port 6379 )
        "Authentication system initializes successfully"))

  (testing "invalid token formats"
    (ok (signals (verify-session-token nil)
            'invalid-token-error)
        "Null token raises invalid-token-error")

    (ok (signals (verify-session-token "")
            'invalid-token-error)
        "Empty token raises invalid-token-error")

    (ok (signals (verify-session-token "not.enough.dots")
            'invalid-token-error)
        "Malformed token raises invalid-token-error")
    )

  (testing "expired tokens"
    (let* ((expired-token (generate-session-token "test-user" '("user")
                                                  :duration -1)))
      (ok (signals (verify-session-token expired-token)
              'session-expired-error)
          "Expired token raised session-expired-error")))

  (testing "role-based access error"
    (let* ((user-token (generate-session-token "test-user" '("user"))))
      (ok (signals (with-auth (user-token :required-roles '("admin"))
                     (declare (ignore result)))
              'permission-denied-error)
          "Accessing admin-only route with user role raises permission-denied-error")))

  (testing "valid token verification"
    (let* ((token (generate-session-token "test-user" '("user"))))
      (ok (verify-session-token token)
          "valid token passes verification")

      (ok (typep (verify-session-token token)))))

  (testing "blacklisted tokens"
    (let* ((token (generate-session-token "test-user" '("user"))))
      ;; Simulate blacklisting the token
      (redis:with-connection ()
        (redis:red-set (format nil "blacklist:~A" token) "1")
        (ok (signals (verify-session-token token)
                'blacklisted-token-error)
            "Blacklisted token raises blacklisted-token-error"))))

)
