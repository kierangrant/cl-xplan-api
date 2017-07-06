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

File: src/api/disclaimer.lisp
Description: /disclaimer API Functions
|#

(in-package :cl-xplan-api/api)

;;; disclaimer/orders

;; disclaimer/orders - GET /resourceful/disclaimer/orders
(define-entrypoint disclaimer/orders :get () () :resource "/disclaimer/orders")

;; disclaimer/orders - PATCH /resourceful/disclaimer/orders
(define-entrypoint disclaimer/orders :patch () (activated content button display_mode)
		   :resource "/disclaimer/orders")

;; disclaimer/orders - POST /resourceful/disclaimer/orders?_method=accept
(define-entrypoint disclaimer/orders :accept () (client_accepted_disclaimer)
		   :single-method :post
		   :single-resource "/disclaimer/orders?_method=accept"
		   :single-parms-as-body T
		   :bulk-resource "/disclaimer/orders")
