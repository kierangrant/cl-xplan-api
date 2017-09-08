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

File: src/api/eapplications/product
Description: /eapplications/product API functions
|#

(in-package :cl-xplan-api/api)

;; eapplications/product - GET /resourceful/eapplications/product and GET /resourceful/eapplications/product/:product_code

(define-entrypoint eapplications/product :get
  (product_code)
  (((include_mappings nil mappings-p) :cond mappings-p :value (if include_mappings 1 0))
   adviser_id client_id  transaction_types product_types)
  :resource (format nil "/eapplications/product~@[~A~]" product_code))
