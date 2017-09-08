#|
This file is part of CL-XPLAN-API, the Lisp XPLAN API Library

Copyright (C) 2017 Kieran Grant
This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser General Public License
(http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Lisp Lesser GNU General Public License for more details.

File: src/api/eapplications/vendor/service.lisp
Description: /eapplications/vendor/service API functions
|#

(in-package :cl-xplan-api/api)

;; eapplications/vendor/service - GET /resourceful/eapplications/vendor/:vendor_code/service and GET /resourceful/eapplications/vendor/:vendor_code/service/:service_code
(define-entrypoint eapplications/vendor/service :get
  (vendor_code service_code) ()
  :resource (format nil "/eapplications/vendor/~A/service~@[/~A~]" vendor_code service_code))

;; eapplications/vendor/service - POST /resourceful/eapplications/vendor/:vendor_code/service
(define-entrypoint eapplications/vendor/service :post
  (vendor_code)
  (((disabled nil disabled-p) :cond disabled-p :value (if disabled 1 0))
   code name locale transaction_types properties)
  :documentation "properties is an array of objects, please construct it with a hash-tables, a-list or p-list, with keys being the string of it's sub-components as per API documentation"
  :resource (format nil "/eapplications/vendor/~A/service" vendor_code))

;; eapplications/vendor/service - PATCH /resourceful/eapplications/vendor/:vendor_code/service/:service_code
(define-entrypoint eapplications/vendor/service :patch
  (vendor_code service_code)
  (((disabled nil disabled-p) :cond disabled-p :value (if disabled 1 0))
   code name locale transaction_types properties)
  :documentation "properties is an array of objects, please construct it with a hash-tables, a-list or p-list, with keys being the string of it's sub-components as per API documentation"
  :resource (format nil "/eapplications/vendor/~A/service/~A" vendor_code service_code))
