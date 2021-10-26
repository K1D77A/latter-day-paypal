;;;; latter-day-paypal.lisp

(in-package #:latter-day-paypal)

;;api-m.paypal.com

(defparameter *testing* nil)


(defapi products%create ("/v1/catalogs/products" post-r)
        ()
        :headers
        (*prefer* *paypal-request-id*))

(defapi tracking%update ("/v1/shipping/trackers/:id" put-r)
        nil)

(defapi tracking%batch-trackers ("/v1/shipping/trackers-batch" post-r)
        nil)

(defapi billing%update ("/v1/payments/billing-agreements/:agreement-id/keker/:coof" patch-r)
        ())

(defapi disputes%get ("/v1/customer/disputes" get-r-query)
        ((start_time
          :accessor start_time
          :initarg :start_time)
         (disputed_transaction_id
          :accessor disputed_transaction_id
          :initarg :disputed_transaction_id)))







