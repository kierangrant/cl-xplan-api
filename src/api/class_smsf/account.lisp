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

File: src/api/class_smsf/account.lisp
Description: /class_smsf/account API Functions
|#

(in-package :cl-xplan-api/api)

;; class_smsf/account - GET /resourceful/class_smsf/account and GET /resourceful/class_smsf/account/:username
(define-entrypoint class_smsf/account :get (username) ((passwd :cond (and username passwd)))
		   :resource (format NIL "/class_smsf/account~@[/~A~]" username))

;; class_smsf/account - POST /resourceful/class_smsf/account
(define-entrypoint class_smsf/account :post () (username password) :resource "/class_smsf/account")

;; class_smsf/account - PUT /resourceful/class_smsf/account/:username
(define-entrypoint class_smsf/account :put (username) (passwd brand)
		   :resource (format nil "/class_smsf/account/~A" username))
