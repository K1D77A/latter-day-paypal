;;;; latter-day-paypal.lisp

(in-package #:latter-day-paypal)

(defparameter *testing* t)


;;;tracking
(defapi tracking%update-or-cancel ("/v1/shipping/trackers/:id" put-r)
        ())

(defapi tracking%information ("/v1/shipping/trackers/:id" get-r)
        ())

(defapi tracking%batch ("/v1/shipping/trackers-batch" post-r)
        ())

;;;billing

(defapi billing%create ("/v1/payments/billing-agreements" post-r)
        ())

(defapi billing%update ("/v1/payments/billing-agreements/:agreement-id" patch-r)
        ())

(defapi billing%information ("/v1/payments/billing-agreements/:agreement-id" get-r)
        ())

(defapi billing%bill-balance
    ("/v1/payments/billing-agreements/:agreement-id/balance" post-r)
    ())

(defapi billing%cancel
    ("/v1/payments/billing-agreements/:agreement-id/cancel" post-r)
    ())

(defapi billing%re-activate
    ("/v1/payments/billing-agreements/:agreement-id/re-activate" post-r)
    ())

(defapi billing%set-balance
    ("/v1/payments/billing-agreements/:agreement-id/set-balance" post-r)
    ())

(defapi billing%suspend
    ("/v1/payments/billing-agreements/:agreement-id/suspend" post-r)
    ())

(defapi billing%list-transactions
    ("/v1/payments/billing-agreements/:agreement-id/transactions" get-r-query)
    ((start_date
      :accessor start-date
      :initarg start-date)
     (end_date
      :accessor end-date
      :initarg :end-date)))

(defapi billing%execute
    ("/v1/payments/billing-agreements/:agreement-id/agreement-execute" post-r)
    ())

;;;catalog products
(defapi products%list ("/v1/catalogs/products" get-r-query)
        ((page_size
          :accessor page-size
          :initarg :page-size)
         (page
          :accessor page
          :initarg :page)
         (total_required
          :accessor total-requried
          :initarg :total-required)))

(defapi products%create ("/v1/catalogs/products" post-r)
        ())
;;has the extra header Prefer and Paypal-Request-Id

(defapi products%update ("/v1/catalogs/products/:product-id" patch-r)
        ())

(defapi products%details ("/v1/catalogs/products/:product-id" get-r)
        ())


;;;disputes
(defapi disputes%get ("/v1/customer/disputes" get-r-query)
        ((start_time
          :accessor start-time
          :initarg :start-time)
         (disputed_transaction_id
          :accessor disputed-transaction_id
          :initarg :disputed-transaction_id)
         (page_size
          :accessor page-size 
          :initarg :page-size)
         (next_page_token
          :accessor next-page-token
          :initarg :next-page-token)
         (dispute_state
          :accessor dispute-state
          :initarg :dispute-state)
         (update_time_before
          :accessor update-time-before
          :initarg :update-time-before)
         (update_time_after
          :accessor update-time-after
          :initarg :update-time-after)))

(defapi disputes%update ("/v1/customer/disputes/:dispute-id" patch-r)
        ())

(defapi disputes%details ("/v1/customer/disputes/:dispute-id" get-r)
        ())









(defapi products%create ("/v1/catalogs/products" post-r)
        ())

(defapi billing%update ("/v1/payments/billing-agreements/:agreement-id/keker/:coof" patch-r)
        ())









