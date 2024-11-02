(defpackage :cl-secure-auth/tests/db
  (:use :cl :rove)
  (:shadowing-import-from :cl-secure-auth
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
                :get-user-roles
                :set-user-roles
                :add-user-role
                :remove-user-role
                :user-has-role-p
                :user-verified
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

(deftest user-update-tests
  (testing "update user email"
    (clean-database)
    (let ((user (create-user "update@example.com" "SecurePass123!")))
      (setf (user-email user) "new@example.com")
      (save-user user)
      (ok (find-user-by-email "new@example.com") "Should find user with updated email")
      (ng (find-user-by-email "update@example.com") "should not find user with old email"))
    )
  )

(deftest role-management-tests
  (testing "default roles"
    (clean-database)
    (let* ((user (create-user "roles@example.com" "SecurePass123!"))
           (roles (get-user-roles user)))
      (ok (member "user" roles :test #'string=) "New user should have the user role")))

  (testing "add role"
    (clean-database)
    (let ((user (create-user "roles@example.com" "SecurePass123!")))
      (add-user-role user "admin")
      (ok (user-has-role-p user "admin") "Should have newly added admin role")
      (ok (user-has-role-p user "user") "Should still have default user role")))

  (testing "remove role"
    (clean-database)
    (let ((user (create-user "roles@example.com" "SecurePass123!")))
      (add-user-role user "admin")
      (remove-user-role user "admin")
      (ng (user-has-role-p user "admin") "Should not have admin role as it was removed")))

  (testing "duplicate role addition"
    (clean-database)
    (let ((user (create-user "roles@example.com" "SecurePass123!")))
      (add-user-role user "admin")
      (add-user-role user "admin")
      (let ((roles (get-user-roles user)))
        (ok (= 1 (count "admin" roles :test #'string=)) "Role should only appear once")))))


(deftest user-verification-tests
  (testing "default verification status"
    (clean-database)
    (let ((user (create-user "unverified@example.com" "SecurePass123!" )))
      (ng (user-verified user) "New user should not be verified by default")))

  (testing "create verified user"
    (clean-database)
    (let ((user (create-user "verified@example.com" "SecurePass123!" :verified t)))
      (ok (user-verified user) "User created with verified flag should be verified")))

  (testing "verify user"
    (clean-database)
    (let ((user (create-user "toverify@example.com" "SecurePass123!")))
      (setf (user-verified user) t)
      (save-user user)
      (let ((found-user (find-user-by-email "toverify@example.com")))
        (ok (user-verified found-user) "User should be verified after setting verified status")))))




