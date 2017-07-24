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

File: src/api/session/password.lisp
Description: /session/password API Functions
|#

(in-package :cl-xplan-api/api)

;; session/password - POST /resourceful/session/password
(define-entrypoint session/password :post () (user_id)
		   :resource "/session/password"
		   :documentation "forgot password")

;; session/password - PATCH /resourceful/session/password
(define-entrypoint session/password :patch
  ()
  (user_id old_password tfa_response new_password)
  :resource "/session/password"
  :documentation "Change password for a user, this is attempted for the currently logged in user, or the user corresponding to the username that must be passed in if no users are logged in.")
