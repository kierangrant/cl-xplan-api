;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
cl-xplan-api:*xplan-session* ; Currenct XPLAN Session. bound with calls to with-xplan-session
cl-xplan-api:*xplan-batched-request* ; Current Batch Request, bound with calls to with-batched-request

xplan-call ; function to make a Non-Batched Request
xplan-add-batch ; function to add a batched call to batched request
xplan-batch-call ; function to make a Batched request

xplan-session-delete ; function to do any closing needed session (if needed, otherwise no-op)
xplan-session-call ; function to make a HTTP request, Generic function on session, standard format

*xplan-http-function* ; variable holding function to make HTTP calls, to be used by xplan-session-call

That is, you specialize xplan-session-call and xplan-session-delete for your custom sessions.
You re-bind *xplan-http-function* to change how the actual underlying HTTP is done.

xplan-call, xplan-add-batch and xplan-batch-call are functions, not generics

;; Process
(defpackage :test (:use :cl :cl-xplan-api))
(in-package :test)
(let ((*xplan-session*
       (make-instance 'xplan-session
		      :base-url "https://SITE.xplan.iress.com.au"
		      :username "user"
		      :password "pass"
		      :api-key "APIKEY")))
  (xplan-call "/session/user" :get)
  (xplan-call $"/entity/client-v3/~!clientid!/assets" :get :parameters '(("page_size" . "100")))
  (xplan-session-delete))

xplan-call will invoke xplan-session-call with appropriately formatted arguments.
xplan-session-call will invoke *xplan-http-function* in a way to include relevant
session details.

*xplan-http-function* will return multiple-values
(RESPONSE RESPONSE-CODE HEADERS (&OPTIONAL RESPONSE-MSG))

RESPONSE must be an (Unsigned-byte 8) array.
Response-code a simple integer for HTTP response code.
HEADERS is an A-List of HTTP Headers returned.
(String for key and value)

xplan-session-call will parse the response, and return
(RESPONSE RESPONSE-CODE HEADERS (&OPTIONAL RESPONSE-MSG))
Possibly stripping any session specifc data, but not required to do so.

XPLAN-CALL and XPLAN-BATCH-CALL will then decode and parse response, based on RESPONSE-CODE and RESPONSE-HEADERS
