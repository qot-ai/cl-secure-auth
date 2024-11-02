(defpackage :cl-secure-auth/tests/db
  (:use :cl :rove)
  (:import-from :cl-secure-auth
                :user
                :user-id
                :user-email
                :user-password-hash
                :user-roles
                :find-user-by-id
                :find-user-by-email
                :save-user
                :validate-email
                :validate-password
                :init-db
                :migrate-database
                :create-user
                :find-user-by-id
                :find-user-by-email
                :user-error
                :ensure-tables))

(in-package :cl-secure-auth/tests/db)

(defparameter *test-db-config*
  '(:database "auth_test"
    :username "postgres"
    :password "1234"
    :host "localhost"
    :port 5432))

(defun clean-database ()
  "Remove all test data"
  (mito:execute-sql "TRUNCATE TABLE users CASCADE"))

(setup
  (apply #'init-db *test-db-config*)
  (clean-database)
  (ensure-tables))

(deftest user-validation-tests
  (testing "email validation"
    (ok (validate-email "user@example.com") "Valid email should pass")
    (ng (validate-email "invalid email") "Invalid email should fail")
    (ng (validate-email "") "Empty email should fail"))

  (testing "password validation"
    (ok (validate-password "SecurePass123!") "Valid password should pass")
    (ng (validate-password "weak") "Short password should fail")
    (ng (validate-password "onlylowercase123!") "password without uppercase should fail")
    (ng (validate-password "ONLYUPPERCASE123!") "password without lowercase should fail")
    (ng (validate-password "NoNumbers!") "Password without numbers should fail")
    (ng (validate-password "NoSpecial123") "Password without special characters should fail")))


(deftest user-creation-tests
  (testing "creating valid user"
    (clean-database)
    (ok (create-user  "test@example.com"  "SecurePass123!")
        "Should create user successfully")
    (ok (find-user-by-email "test@example.com")
        "Should find created user"))
  (testing "duplicate email rejection"
    (clean-database)
    (create-user "duplicate@example.com" "SecurePass123!")
    (ok (signals (create-user "duplicate@example.com" "SecurePass123!") 'user-error)
        "Should reject duplicate email")))

(deftest user-find-tests
  (testing "find by email"
           (clean-database)
           (let ((user (create-user "find@example.com" "SecurePass123!")))
             (ok (find-user-by-email "find@example.com") "Should find existing user")
             (ok (find-user-by-email "FIND@EXAMPLE.COM") "Should be case insensititve")
             (ng (find-user-by-email "notfound@example.com") "Should return nil for non-existing user")))
  (testing "find by id"
    (clean-database)
    (let* ((user (create-user "findid@example.com" "SecurePass123!"))
           (user-id (user-id user)))
      (ok (find-user-by-id user-id) "Should find user by id")
      (ng (find-user-by-id (uuid:make-v4-uuid)) "Should return nil for non existing user id"))))




