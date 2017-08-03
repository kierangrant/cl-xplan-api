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

File: src/api/eapplications/vendor/product.lisp
Description: /eapplications/vendor/product API functions
|#

(in-package :cl-xplan-api/api)

;; GET /resourceful/eapplications/vendor/:vendor_code/product and GET /resourceful/eapplications/vendor/:vendor_code/product/:product_code
(define-entrypoint eapplications/vendor/product :get
  (vendor_code product_code) ()
  :resource (format nil "/eapplications/vendor/~A/product~@[/~A~]" vendor_code product_code))

;; POST /resourceful/eapplications/vendor/:vendor_code/product
(define-entrypoint eapplications/vendor/product :post
  (vendor_code)
  (((is_lite nil is_lite-p) :cond is_lite-p :value (if is_lite 1 0))
   ((is_stp nil is_stp-p) :cond is_stp-p :value (if is_stp 1 0))
   ((stp_allowed nil stp_allowed-p) :cond stp_allowed-p :value (if stp_allowed 1 0))
   ((disabled nil disabled-p) :cond disabled-p :value (if disabled 1 0))
   ((allowed nil allowed-p) :cond allowed-p :value (if allowed 1 0))
   ((licence_required nil licence_required-p) :cond licence_required-p :value (if licence_required 1 0))
   ((is_passthrough nil is_passthrough-p) :cond is_passthrough-p :value (if is_passthrough 1 0))   
   code name product_type locale entity_types transaction_types properties services statuses user_groups sub_type
   last_pds_update vendor_product_code external_system_codes)
  :documentation
  "properties, services, statuses, user_groups and external_system_codes are arrays of objects, please construct them with hash-tables, a-lists or p-lists, with keys being the string of their sub-components as per API documentation"
  :resource (format nil "/eapplications/vendor/~A/product" vendor_code))

;; PATCH /resourceful/eapplications/vendor/:vendor_code/product/:product_code
(define-entrypoint eapplications/vendor/product :patch
  (vendor_code product_code)
  (((is_lite nil is_lite-p) :cond is_lite-p :value (if is_lite 1 0))
   ((is_stp nil is_stp-p) :cond is_stp-p :value (if is_stp 1 0))
   ((stp_allowed nil stp_allowed-p) :cond stp_allowed-p :value (if stp_allowed 1 0))
   ((disabled nil disabled-p) :cond disabled-p :value (if disabled 1 0))
   ((allowed nil allowed-p) :cond allowed-p :value (if allowed 1 0))
   ((licence_required nil licence_required-p) :cond licence_required-p :value (if licence_required 1 0))
   ((is_passthrough nil is_passthrough-p) :cond is_passthrough-p :value (if is_passthrough 1 0))
   code name product_type locale entity_types transaction_types properties services statuses user_groups sub_type
   last_pds_update vendor_product_code external_system_codes)
  :documentation
  "properties, services, statuses, user_groups and external_system_codes  are arrays of objects, please construct them with hash-tables, a-lists or p-lists, with keys being the string of their sub-components as per API documentation"
  :resource (format nil "/eapplications/vendor/~A/product/~A" vendor_code product_code))
