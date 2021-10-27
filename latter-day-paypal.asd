;;;; latter-day-paypal.asd

(asdf:defsystem #:latter-day-paypal
  :description "Paypal api wrapper."
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "api"
  :depends-on (#:closer-mop
               #:str 
               #:jonathan
               #:cl-json
               #:dexador
               #:do-urlencode
               #:local-time)
  :components ((:file "package")
               (:file "conditions")
               (:file "protocol")
               (:file "response")
               (:file "token")
               (:file "latter-day-paypal")
               (:file "helpers")))

