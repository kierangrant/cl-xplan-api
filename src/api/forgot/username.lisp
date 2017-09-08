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

File: src/api/forgot/username.lisp
Description: /forgot/username API functions
|#

(in-package :cl-xplan-api/api)

;; forgot/username - POST /resourceful/forgot/username
(define-entrypoint forgot/username :post
  () (preferred_email role_type first_name last_name dob)
  :resource "/forgot/username"
  :documentation "Send user_id to the email address if that email address is the preferred email of an entity with specified role type. Otherwise raise error.")
