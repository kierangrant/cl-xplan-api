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

File: src/api/eapplications/vendor.lisp
Description: /eapplications/vendor API functions
|#

(in-package :cl-xplan-api/api)

;; GET /resourceful/eapplications/vendor and GET /resourceful/eapplications/vendor/:vendor_code
(define-entrypoint eapplications/vendor :get
  (vendor_code) ()
  :resource (format nil "/eapplications/vendor~@[/~A~]" vendor_code))

;; POST /resourceful/eapplications/vendor
(define-entrypoint eapplications/vendor :post
  () (code name properties)
  :documentation "properties is an array of object with parameters 'type', 'code' and 'value'. Use Hash-table, a-list or a p-list to construct this"
  :resource "/eapplications/vendor")

;; PATCH /resourceful/eapplications/vendor/:vendor_code
(define-entrypoint eapplications/vendor :patch
  (vendor_code) (code name properties)
  :documentation "properties is an array of object with parameters 'type', 'code' and 'value'. Use Hash-table, a-list or a p-list to construct this"
  :resource (format nil "/eapplications/vendor/~A" vendor_code))

