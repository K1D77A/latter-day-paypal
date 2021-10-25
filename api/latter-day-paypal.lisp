;;;; latter-day-paypal.lisp

(in-package #:latter-day-paypal)

;;api-m.paypal.com

(defparameter *api* "https://api-m.sandbox.paypal.com")

(defclass request ()
  ((paypal-auth-assertion
    :accessor paypal-auth-assertion
    :initarg :paypal-auth-assertion)
   (paypal-client-metadata-id
    :accessor paypal-client-metadata-id
    :initarg :paypal-client-metadata-id)
   (paypal-partner-attribution-id
    :accessor paypal-partner-attribution-id
    :initarg :paypal-partner-attribution-id)
   (paypal-request-id
    :accessor paypal-request-id
    :initarg :paypal-request-id)))

(defclass get-request (request)
  ((q%count
    :accessor q%count
    :initarg :q%count)
   (q%end-time 
    :accessor q%end-time
    :initarg :q%end-time)
   (q%page
    :accessor q%page
    :initarg :q%page)
   (q%page-size 
    :accessor q%page-size
    :initarg :q%page-size)
   (q%total-count-required 
    :accessor q%total-count-required
    :initarg :q%total-count-required)
   (q%sort-by
    :accessor q%sort-by
    :initarg :q%sort-by)
   (q%sort-order
    :accessor q%sort-order
    :initarg :q%sort-order)
   (q%start-id
    :accessor q%start-id
    :initarg :q%start-id)
   (q%start-index
    :accessor q%start-index
    :initarg :q%start-index)
   (q%start-time
    :accessor q%start-time
    :initarg :q%start-time)))

(defgeneric generate-headers (request)
  (:method-combination append :most-specific-last)
  (:documentation "Generate the headers for a request using the APPEND method combination"))


(defmethod generate-headers (request)
  (with-accessors ((paypal-auth-assertion paypal-auth-assertion)
                   (paypal-client-metadata-id paypal-client-metadata-id)
                   (paypal-partner-attribution-id paypal-partner-attribution-id)
                   (paypal-request-id paypal-request-id))
      request 
    (is-token-bound)
    (is-expired-token *token*)
    (append
     `(("Content-Type" . "application/json")
       ("Authorization" . (format nil "Bearer ~A"
                                  (access-token *token*))))
     (when (slot-boundp request 'paypal-auth-assertion)
       (cons "PayPal-Auth-Assertion" paypal-auth-assertion))
     (when (slot-boundp request 'paypal-client-metadata-id)
       (cons "PayPal-Client-Metadata-Id" paypal-client-metadata-id))
     (when (slot-boundp request 'paypal-partner-attribution-id)
       (cons "PayPal-Partner-Attribution-Id" paypal-partner-attribution-id))
     (when (slot-boundp request 'paypal-request-id)
       (cons "PayPal-Request-Id" paypal-request-id)))))

(defun encode-slot (stream request slot)
  (let ((name (c2mop:slot-definition-name slot)))
    (when (slot-boundp request name)
      (let ((clean (str:snake-case (subseq (string-downcase name) 2))))
        (format stream "~A=~A" clean (slot-value request name))))))

(defmethod encode-query-params ((request get-request))
  (let ((stream (make-string-output-stream)))
    (macrolet ((qe (name encoded)
                 `(when (slot-boundp request ',name)
                    (format stream "~A=~A&" ,encoded (slot-value request ',name)))))
      (format stream "?")
      (qe q%count "count")
      (qe q%end-time "end_time")
      (qe q%page "page")
      (qe q%page-size "page_size")
      (qe q%total-count-required "total_count_required")
      (qe q%sort-by "sort_by")
      (qe q%sort-order "sort_order")
      (qe q%start-id "start_id")
      (qe q%start-index "start_index")
      (qe q%start-time "start_time"))
    (let ((output (get-output-stream-string stream)))
      (if (string= output "?")
          ""
          (subseq output 0 (1- (length output)))))))








