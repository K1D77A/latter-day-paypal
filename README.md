# latter-day-paypal
Right now this is just a thin wrapper over the paypal api.

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


## License

MIT

