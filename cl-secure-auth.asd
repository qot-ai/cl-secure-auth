(defsystem "cl-secure-auth"
  :version "0.1.0"
  :author "QOT"
  :license "MIT"
  :description "A secure authentication library for Common Lisp"
  :depends-on ("jose"
               "cl-redis"
               "cl-ppcre"
               "local-time"
               "uuid"
               "mito"
               "mito-migration"
               "mito-auth"
               "sxql"
               "cl-json"
               "cl-argon2")
  :components ((:module "src"
                :components
                        ((:file "package")
                         (:file "utils")
                         (:file "conditions")
                         (:file "redis" )
                         (:file "core" )
                         (:file "session")
                         (:file "user")
                         (:file "token")
                         (:file "db")
                         )))
  :in-order-to ((test-op (test-op "cl-secure-auth/tests"))))

(defsystem "cl-secure-auth/tests"
  :author "QOT"
  :license "MIT"
  :description "Test system for cl-secure-auth"
  :depends-on ("cl-secure-auth"
               "rove")
  :components ((:module "tests"
                :components
                        ((:file "core")
                         (:file "db-test")
                         (:file "session-tests"))))
  :perform (test-op (o c)
                    (symbol-call :rove :run-suite :cl-secure-auth/tests/core)
                    (symbol-call :rove :run-suite :cl-secure-auth/tests/db)
                    (symbol-call :rove :run-suite :cl-secure-auth/tests/session)
                    ))
