;;;; latter-day-paypal.asd

(asdf:defsystem #:latter-day-paypal
  :description "Paypal api wrapper."
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.3"
  :serial t
  :pathname "api"
  :depends-on (#:closer-mop
               #:str 
               #:jonathan
               #:ironclad
               #:quri
               #:cl-tls
               #:dexador
               #:cl-base64
               #:hunchentoot
               #:ningle
               #:do-urlencode
               #:local-time)
  :components ((:file "package")
               (:file "conditions")
               (:file "protocol")
               (:file "response")
               (:file "token")
               (:file "latter-day-paypal")
               (:file "webhook-verify")
               (:file "helpers")))

