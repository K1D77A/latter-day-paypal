# No longer maintained please see https://github.com/K1D77A/lisp-pay

# latter-day-paypal
Right now this is just a thin wrapper over the paypal api.

## Dependencies

You will need to git clone cl-tls from  https://github.com/shrdlu68/cl-tls and put it into your local-projects, this is for verifying webhooks.

To adjust how jojo parses, set the parameter `*parse-as*` to either :hash-table or :plist,
by default it is :plist to maintain backwards compatibility. 

To adjust the content encoder, set the parameter `*json-encoder*` to a function, by 
default it is `#'cl-json:encode-json-as-string`. 

## Intro

To get started you need to set `*client*` and `*secret*` to your client and secret. 
Then call `get-token`. They are currently set to my old client and secret.

```lisp
LDP> *client*
"ATiiZbWBH3_qd_y3P3AZQiQlBIh9mVTDSTtr4ALOPqfTd5eBZooqeJlLT0o6-HLF95_Vj2GADaIhp5Ee"
LDP> *secret*
"EMBuo5-J3kWfSEJYY5mtQd8Hm9JezbxjkUUJ2D9JwKwwas1E05Ejp4A1wlpNuuFd3YyIoKZrSxjs9OUb"
LDP> (get-token)
#<TWO-HUNDRED {100A51E5C3}>
#<TOKEN {100A456B93}>
LDP> 
```
This sets the value of `*token*`. 
Now you have your token you can make requests. 
```lisp
LDP> (make-instance 'products%list)
#<PRODUCTS%LIST {100BBA0F7B}>
LDP> (call-api *)
#<TWO-HUNDRED {100BD2E673}>
LDP> (body *)
(:|links|
 ((:|method| "GET" :|rel| "self" :|href|
   "https://api.sandbox.paypal.com/v1/catalogs/products?page_size=10&page=1"))
 :|products| NIL)
LDP> 
```
The result is wrapped in an object with its status code and text and a body slot that 
contains the result. The same is true if it returns an error.
The json is decoded using jonathan.

## Token issues
If you have failed to set token or it has expired
```lisp
LDP> (setf *token* nil)
NIL
LDP> (make-instance 'products%list)
#<PRODUCTS%LIST {100BD37F5B}>
LDP> (call-api *)
; Debugger entered on #<UNBOUND-TOKEN {100BD3E133}>

You have not evaluated 'get-token'.
   [Condition of type UNBOUND-TOKEN]

Restarts:
 0: [MISSING-TOKEN] Token could be broken, refresh and try again?
```
All requests made with `call-api` have the restart `missing-token` just in case your token expires. 

```lisp
#<PRODUCTS%LIST {100BD37F5B}>
LDP> (handler-bind ((token-issue (lambda (c)
                                   (declare (ignore c))
                                   (invoke-restart 'missing-token))))
       (call-api *))
#<TWO-HUNDRED {1003A50663}>
LDP> (body *)
(:|links|
 ((:|method| "GET" :|rel| "self" :|href|
   "https://api.sandbox.paypal.com/v1/catalogs/products?page_size=10&page=1"))
 :|products| NIL)
LDP> 
```

## Errors
All conditions are subclasses of `paypal-api-condition` see conditions.lisp
```lisp
LDP> (handler-case (call-api *)
       (condition (c)
         c))
#<FOUR-HUNDRED-FOUR Status: 404.
Status Text: Not Found.
Body: NIL {10040F0083}>
LDP> 
```

## Adding headers

Some calls accept other headers like Paypal-Request-Id to add these headers to a request lexically bind the variable `*request-headers*`

```lisp
#<PRODUCTS%LIST {101130BC0B}>
LDP> (let ((*request-headers* '(("Paypal-Auth-Assertion" . "imauthassertion"))))
       (declare (special *request-headers*))
       <request> 
       <call-api>)
```
Headers are sent using Dex so they have to be a properly formed alist like above.
You can see the additional headers in the paypal dev docs.

## Testing 
By default the API URL used is the sandbox url, to go live set `*testing*` to non nil.
```lisp
LDP> *testing*
T
```

## Encoding

All encoding is done with cl-json. So the easiest way to create JSON objects is to use 
a hash-table. There is a helper function called `%quick-hash` to generate a hash-table from an alist
```lisp
LDP> (cl-json:encode-json-to-string (%quick-hash '(("abc" . "def"))))
"{\"abc\":\"def\"}"
LDP> 
```
Patch requests take an array:

```lisp
LDP> (make-array 1 :initial-element (%quick-hash '(("abc" . "def"))))
#(#<HASH-TABLE :TEST EQL :COUNT 1 {100BED8343}>)
LDP> (cl-json:encode-json-to-string *)
"[{\"abc\":\"def\"}]"
LDP> 
```

## Patch requests

A lot of the requests that update are patch requests which accept objects. When you are using make-instance you will see a slot called 'patch-request, put your request data in this.

## Requests that have a json body

Most requests (put/post) have a body, to provide this data use the :content slot. 

## Query parameters

Query parameters are slots within the object, just set them and the ones that are bound will be encoded and added onto the end of the URL.

## Path parameters

Path parameters are slots within the request object, just set the slots and they will be automatically encoded into the URL.

## Webhook verification
To verify the signature of a paypal request there are two methods you can use. 
`(ldp:verify-webhook )` this takes `algo cert-url transmission-signature transmission-id timestamp webhook-id raw-body` algo is a keyword generated with `%algo->key` its simply the string converted to a keyword.
Or you can use 
`(ldp:verify-paypal-webhook)` which takes `webhook-id request raw-body` this is a method that will dispatch on REQUEST, and currently only works with a hunchentoot request object like so: 
```lisp
(hunchentoot:define-easy-handler (paypal-payment-processor
                                  :uri <your webhook url>
                                  :default-request-type :POST)
    ()
  (let* ((raw-data (tbnl:raw-post-data :force-binary t)))
    (if (ldp:verify-paypal-webhook (if *testing*
                                       "your testing webhook id"
                                       "Your live webhook id")
                                   tbnl:*request* raw-data)
        (let ((plist (jojo:parse (babel:octets-to-string raw-data))))
          (setf (tbnl:return-code*) 200)
          <your processing method> 
          "t")
        (progn (setf (tbnl:return-code*) 400)
               "nil"))))
```
If you are using a server that is not Hunchentoot then you can just extract the header values and pass them to `(ldp:verify-webhook)`, this is all the method `ldp:verify-paypal-webhook` is doing under the hood. 


## License

MIT

