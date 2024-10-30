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
    :password "1234"
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

(deftest user-crud-tests
  (testing "user creation and retrieval"
    (let* ((test-user (make-instance 'user
                                    :id "test-123"
                                    :email "test@example.com"
                                    :password-hash "hashed-password"
                                    :roles '("user" "admin")))
           (saved-user (save-user test-user)))
      
      (ok saved-user "User should be saved successfully")
      (ok (typep saved-user 'user) "Saved object should be a user instance")
      
      (let ((found-user (find-user-by-id "test-123")))
        (ok found-user "Should find user by ID")
        (ok (string= (user-email found-user) "test@example.com")
            "Found user should have correct email")
        (ok (equal (user-roles found-user) '("user" "admin"))
            "Found user should have correct roles"))
      
      (let ((found-by-email (find-user-by-email "test@example.com")))
        (ok found-by-email "Should find user by email")
        (ok (string= (user-id found-by-email) "test-123")
            "Found user should have correct ID"))))
  
  (testing "user update"
    (let* ((user (find-user-by-id "test-123"))
           (updated-user (progn
                          (setf (user-email user) "updated@example.com")
                          (save-user user)))
           (retrieved-user (find-user-by-id "test-123")))
      (ok (string= (user-email retrieved-user) "updated@example.com")
          "User email should be updated"))))

(deftest database-connection-tests
  (testing "database initialization"
    (ok (progn
          (apply #'init-db *test-db-config*)
          t)
        "Should connect to test database")
    
    (ok (progn
          (ensure-tables)
          t)
        "Should create required tables")))


