
(in-package #:latter-day-paypal)

(defclass api-slot (c2mop:slot-definition)
  ((name
    :accessor name
    :initarg :name)
   (name->json
    :accessor name->json 
    :initarg :name->json
    :initform nil)))

(defclass api-call (c2mop:funcallable-standard-class)
  ((endpoint
    :accessor endpoint
    :initarg :endpoint
    :documentation "A URL with :<slot-name> within where the slot value is encoded.")
   (string-constructor
    :accessor string-constructor
    :initarg :string-constructor
    :documentation "A function that returns a generated URL.")
   (genned-slot-names
    :accessor genned-slot-names
    :initarg :genned-slot-names)
   (query-slot-names
    :accessor query-slot-names
    :initarg :query-slot-names)
   (query-constructor
    :accessor query-constructor
    :initarg :query-constructor)
   (headers
    :accessor headers
    :initarg :headers)))


(defclass api-slot-direct (api-slot c2mop:standard-direct-slot-definition)
  ())

(defclass api-slot-effective (api-slot c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:validate-superclass ((class api-call)
                                      (metaclass c2mop:funcallable-standard-class))
  t)

(defmethod c2mop:validate-superclass ((class api-slot)
                                      (metaclass standard-class))
  t)

(defmethod c2mop:effective-slot-definition-class ((class api-call) &rest initargs)
  (find-class 'api-slot-effective))

(defmethod c2mop:direct-slot-definition-class ((class api-call) &rest initargs)
  (find-class 'api-slot-direct))

(defclass request ()
  (;; (paypal-auth-assertion
   ;;  :accessor paypal-auth-assertion
   ;;  :initarg :paypal-auth-assertion)
   ;; (paypal-client-metadata-id
   ;;  :accessor paypal-client-metadata-id
   ;;  :initarg :paypal-client-metadata-id)
   ;; (paypal-partner-attribution-id
   ;;  :accessor paypal-partner-attribution-id
   ;;  :initarg :paypal-partner-attribution-id)
   ;; (paypal-request-id
   ;;  :accessor paypal-request-id
   ;;  :initarg :paypal-request-id)
   (request-fun
    :reader request-fun
    :initarg :request-fun
    :initform 'dex:get))
  (:metaclass api-call))

(defclass request-without-content (request)
  ()
  (:metaclass api-call))

(defclass get-r (request-without-content)
  ((request-fun :initform 'dex:get))
  (:metaclass api-call))

(defclass query-req (request-without-content)
  ()
  (:metaclass api-call))

(defclass get-r-query (query-req)
  ()
  (:metaclass api-call))

(defclass delete-r  (request-without-content)
  ((request-fun :initform 'dex:delete))
  (:metaclass api-call))

(defclass request-with-content (request)
  ((content
    :accessor content
    :initarg :content
    :type list))
  (:metaclass api-call))

(defclass patch-r (request)
  ((request-fun :initform 'dex:patch)
   (patch-request
    :accessor patch-request
    :initarg :patch-request))
  (:metaclass api-call))

(defclass post-r (request-with-content)
  ((request-fun :initform 'dex:post))
  (:metaclass api-call))

(defclass post-files-request (post)
  ()
  (:metaclass api-call))

(defclass put-r (request-with-content)
  ((request-fun :initform 'dex:put))
  (:metaclass api-call))

(defun in-list (obj)
  (if (listp obj)
      (first obj)
      obj))

(defun replace-vars-for-slot-names (split slots)
  (mapcar (lambda (str)
            (let ((found?
                    (find (subseq str 1) slots :test #'string-equal)))
              (if found?
                  found?
                  str)))
          split))

(defun gen-url-generator (class)
  (with-accessors ((endpoint endpoint)
                   (genned-slot-names genned-slot-names))
      class 
    (let* ((split (str:split #\/ (in-list endpoint) :omit-nulls t))
           (slots (in-list (genned-slot-names class)))
           (compared (replace-vars-for-slot-names split slots)))
      (if slots 
          (compile nil
                   `(lambda (request)
                      (format nil "/~{~A~^/~}"
                              (loop :for slot? :in ',compared
                                    :collect
                                    (if (stringp slot?)
                                        slot? 
                                        (do-urlencode:urlencode
                                         (slot-value request slot?)))))))
          (lambda (request)
            (declare (ignore request))
            (in-list endpoint))))))

(defun gen-query-generator (class)
  (with-accessors ((query-slot-names query-slot-names))
      class 
    (let* ((slots (in-list query-slot-names)))
      (compile nil
               `(lambda (request)
                  (format nil "?~{~A~^&~}"
                          (loop :for slot :in ',slots
                                :if (slot-boundp request slot)
                                  :collect (format nil "~A=~A"
                                                   (string-downcase (symbol-name slot))
                                                   (do-urlencode:urlencode
                                                    (slot-value request slot))))))))))

(defun slots-from-url (url)
  (let* ((split (str:split #\/ url :omit-nulls t))
         (slots (remove-if-not (lambda (ele) (char= #\: (aref ele 0))) split)))
    (mapcar (lambda (slot)
              (let* ((name (subseq slot 1))
                     (upcase (string-upcase name))
                     (intern (intern upcase))
                     (key (intern upcase :keyword)))
                (list intern :accessor intern :initarg key)))
            slots)))

(defmacro defapi (name (endpoint super) query-slots &key (headers nil))
  (let* ((slots (slots-from-url endpoint))
         (names (mapcar #'first slots))
         (query-slot-names (mapcar #'first query-slots)))
    `(let ((class 
             (defclass ,name (,super)
               ,(append slots query-slots)
               ,@(append `((:metaclass api-call)
                           (:genned-slot-names ,names)
                           (:query-slot-names ,query-slot-names)
                           (:endpoint ,endpoint)
                           (:headers ,headers))))))
       (c2mop:ensure-finalized class)
       (with-slots (string-constructor headers query-constructor)
           class
         (setf (string-constructor class) (gen-url-generator class))
         (when ',query-slots
           (setf (query-constructor class) (gen-query-generator class)))
         ,(when headers 
            `(defmethod generate-headers append ((req ,name))
               (declare (special ,@headers))
               (append ,@(mapcar (lambda (special)
                                   `(when (boundp ',special)
                                      ,special))
                                 headers))))))))

(c2mop:define-method-combination string-gen (&optional (order ':most-specific-last))
  ((around (:around))
   (primary (string-gen)))
  (case order
    (:most-specific-first)
    (:most-specific-last (setq primary (reverse primary))))
  (let ((form (if (rest primary)
                  `(concatenate 'string ,@(mapcar #'(lambda (method)
                                                      `(call-method ,method))
                                                  primary))
                  `(call-method ,(first primary)))))

    (if around
        `(call-method ,(first around)
                      (,@(rest around))
                      (make-method ,form))
        form)))

(defgeneric generate-url (req)
  (:method-combination string-gen :most-specific-last))

(defmethod generate-url string-gen (req)
  (if *testing*
      "https://api-m.sandbox.paypal.com"
      "https://api-m.paypal.com"))

(defmethod generate-url string-gen ((req request))
  (funcall (in-list (string-constructor (class-of req))) req))

(defmethod generate-url string-gen ((req query-req))
  (funcall (in-list (query-constructor (class-of req))) req))

(defmethod extra-headers (req)
  nil)

(defmethod extra-headers ((req request))
  (in-list (slot-value (class-of req) 'headers)))

(defgeneric generate-headers (req)
  (:method-combination append :most-specific-last))

(defmethod generate-headers :around (req)
  (is-token-non-nil)
  (is-token-bound)
  (is-expired-token *token*)
  `(:headers ,(call-next-method)))

(defmethod generate-headers append (req)
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(format nil "Bearer ~A" (access-token *token*)))))

(defmethod generate-content (req)
  nil)

(defmethod generate-content ((req request-with-content))
  `(:content ,(cl-json:encode-json-to-string (slot-value req 'content))))

(defmethod generate-content ((req patch-r))
  `(:content ,(cl-json:encode-json-to-string (slot-value req 'patch-request))))

(defmethod generate-dex-list (req)
  (append (generate-headers req) (generate-content req)))

(defgeneric call-api (req))

(defmethod call-api (req)
  (flet ((body (req)
           (let ((url (generate-url req))
                 (args (generate-dex-list req))
                 (fun (request-fun req)))
             (wrapped-dex-call (resp status)
               (apply fun url args)
               (make-instance (determine-good-class status)
                              :body (jojo:parse resp))))))
    (restart-case 
        (body req)
      (missing-token ()
        :report "Token could be broken, refresh and try again?"
        (get-token)
        (body req)))))



