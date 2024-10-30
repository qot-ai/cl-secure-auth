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
                :ensure-tables))

(in-package :cl-secure-auth/tests/db)

(defparameter *test-db-config*
  '(:database "auth_test"
    :username "postgres"
    :password "postgres"
    :host "localhost"
    :port 5432))

(setup
  (apply #'init-db *test-db-config*)
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




