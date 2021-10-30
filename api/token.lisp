(in-package #:latter-day-paypal)

(defparameter *token* nil)

(defparameter *client* "ATiiZbWBH3_qd_y3P3AZQiQlBIh9mVTDSTtr4ALOPqfTd5eBZooqeJlLT0o6-HLF95_Vj2GADaIhp5Ee")

(defparameter *secret* "EMBuo5-J3kWfSEJYY5mtQd8Hm9JezbxjkUUJ2D9JwKwwas1E05Ejp4A1wlpNuuFd3YyIoKZrSxjs9OUb")

(defclass token ()
  ((nonce
    :reader nonce
    :initarg :nonce)
   (expires-in
    :reader expires-in
    :initarg :expires-in
    :type local-time:timestamp)
   (app-id
    :reader app-id
    :initarg :app-id)
   (token-type
    :reader token-type
    :initarg :token-type)
   (access-token
    :reader access-token
    :initarg :access-token)
   (scope
    :reader scope
    :initarg :scope)))

(defun token-plist->token (token-plist)
  (destructuring-bind (&key |nonce| |expires_in| |app_id| |token_type| |access_token|
                         |scope| &allow-other-keys)
      token-plist
    (make-instance 'token :scope |scope| :access-token |access_token|
                          :nonce |nonce| :expires-in
                          (local-time:timestamp+ (local-time:now) |expires_in| :sec)
                          :app-id |app_id| :token-type |token_type|)))

(defun get-token ()
  (wrapped-dex-call (resp status)
    (dex:post (format nil "~A/v1/oauth2/token"
                      (generate-url t))
              :basic-auth `(,*client* . ,*secret*)
              :headers '(("Accept" . "application/json")
                         ("Accept-Language" . "en_US"))
              :content '(("grant_type" . "client_credentials")))
    (let ((token (token-plist->token
                  (jojo:parse resp))))
      (values 
       (make-instance (determine-good-class status) :body token)
       (setf *token* token)))))


(defmethod expiredp ((token token))
  "Checks to see if the token has expired."
  (with-accessors ((expires-in expires-in))
      token 
    (let* ((now (local-time:now)))
      (local-time:timestamp<= expires-in now))))

(defun is-token-non-nil ()
  (unless *token*
    (error 'unbound-token)))

(defmethod is-expired-token ((token token))
  (when (expiredp token)
    (error 'expired-token :token token)))

(defmethod is-token-bound ()
  (unless (boundp '*token*)
    (error 'unbound-token)))
