(defpackage :cl-secure-auth
  (:use :cl)
  (:import-from :redis)
  (:import-from :jose)
  (:export :initialize-auth
   :generate-session-token
           :verify-session-token
   :with-auth
           :has-role-p
   :auth-error
           :invalid-token-error
   :session-expired-error
           :permission-denied-error
   :blacklisted-token-error
           :token-blacklisted-p
   :blacklist-token
           :logout
   :store-session-data
           :get-session-data
   :delete-session-data))
