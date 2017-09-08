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

File: src/api/eapplications/vendor/product/option.lisp
Description: /eapplications/vendor/product/option API functions
|#

(in-package :cl-xplan-api/api)

;; eapplications/vendor/product/option - GET /resourceful/eapplications/vendor/:vendor_code/product/:product_code/option and GET /resourceful/eapplications/vendor/:vendor_code/product/:product_code/option/:option_code
(define-entrypoint eapplications/vendor/product/option :get
  (vendor_code product_code option_code)
  ()
  :resource (format nil "/eapplications/vendor/~A/product/~A/option~@[/~A~]" vendor_code product_code option_code))

;; eapplications/vendor/product/option - POST /resourceful/eapplications/vendor/:vendor_code/product/:product_code/option
(define-entrypoint eapplications/vendor/product/option :post
  (vendor_code product_code)
  (((disabled nil disabled-p) :cond disabled-p :value (if disabled 1 0))
   code name properties)
  :documentation "properties is an array of objects, please construct it with a hash-tables, a-list or p-list, with keys being the string of it's sub-components as per API documentation"
  :resource (format nil "/eapplications/vendor/~A/product/~A/option" vendor_code product_code))

;; eapplications/vendor/product/option - PATCH /resourceful/eapplications/vendor/:vendor_code/product/:product_code/option/:option_code
(define-entrypoint eapplications/vendor/product/option :patch
  (vendor_code product_code option_code)
  (((disabled nil disabled-p) :cond disabled-p :value (if disabled 1 0))
   code name properties)
  :documentation "properties is an array of objects, please construct it with a hash-tables, a-list or p-list, with keys being the string of it's sub-components as per API documentation"
  :resource (format nil "/eapplications/vendor/~A/product/~A/option/~A" vendor_code product_code option_code))

