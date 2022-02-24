(in-package #:latter-day-paypal)

(defclass response ()
  ((body
    :accessor body
    :initarg :body
    :type list)
   (status-code
    :accessor status-code
    :initarg :status-code
    :type fixnum)
   (status-string
    :accessor status-string
    :initarg :status-string
    :type string)))

(defclass good-response (response)
  ())

(defclass two-hundred (good-response)
  ((status-code :initform 200)
   (status-string :initform "OK")))

(defclass two-hundred-one (good-response)
  ((status-code :initform 201)
   (status-string :initform "Created")))

(defclass two-hundred-two (good-response)
  ((status-code :initform 202)
   (status-string :initform "Accepted")))

(defclass two-hundred-four (good-response)
  ((status-code :initform 204)
   (status-string :initform "No Content")))

(defun determine-good-class (n)
  (case n
    (200 'two-hundred)
    (201 'two-hundred-one)
    (202 'two-hundred-two)
    (204 'two-hundred-four)))


