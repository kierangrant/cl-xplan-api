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

File: src/api/eapplications/product_type.lisp
Description: /eapplications/product_type API functions
|#

(in-package :cl-xplan-api/api)

;; eapplications/product_type - GET /resourceful/eapplications/product_type and GET /resourceful/eapplications/product_type/:type_id
(define-entrypoint eapplications/product_type :get
  (type_id)
  ()
  :resource (format nil "/eapplications/product_type~@[/~A~]" type_id))

;; eapplications/product_type - POST /resourceful/eapplications/product_type
(define-entrypoint eapplications/product_type :post
  ()
  (eapplications_type xplan_type locale ((disabled nil disabled-p) :cond disabled-p :value (if disabled 1 0)))
  :resource "/eapplications/product_type")

;; eapplications/product_type - PATCH /resourceful/eapplications/product_type/:type_id
(define-entrypoint eapplications/product_type :patch
  (type_id)
  (eapplications_type xplan_type locale ((disabled nil disabled-p) :cond disabled-p :value (if disabled 1 0)))
  :resource (format nil "/eapplications/product_type/~A" type_id))
