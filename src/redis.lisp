(in-package :cl-secure-auth)

(defmacro with-redis-connection (() &body body)
  "Execute body with a Redis connection"
  `(redis:with-connection ()
     ,@body))

(defun store-session-data (token data &optional ttl)
  "Store session data in Redis"
  (with-redis-connection ()
    (redis:red-set (format nil "session:~A" token)
               (prin1-to-string data))
    (when ttl
      (redis:red-expire (format nil "session:~A" token) ttl))))

(defun get-session-data (token)
  "Retrieve session data from Redis"
  (with-redis-connection ()
    (let ((data (redis:red-get (format nil "session:~A" token))))
      (when data
        (read-from-string data)))))

(defun delete-session-data (token)
  "Delete session data from Redis"
  (with-redis-connection ()
    (redis:red-del (format nil "session:~A" token))))
