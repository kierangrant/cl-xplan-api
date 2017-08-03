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

File: src/api/forgotten_password.lisp
Description: /forgotten_password API functions
|#

(in-package :cl-xplan-api/api)

#| General Notes:
API password reset process for advisers as well as non-standard users such as client access users or referrers.
Note that regardless of differences between reset processes of different user types in the frontend, all password reset journeys in the API follow the same process:
1. POST call to acquire a reset code via email
2. GET call with the reset code to acquire the secret questions
3. PATCH call with the answers to the secret questions and the new password
|#

;; GET /resourceful/forgotten_password
(define-entrypoint forgotten_password :get
  () (user_id reset_code)
  :resource "/forgotten_password"
  :documentation "Given the reset code, return a given user's secret questions")

;; POST /resourceful/forgotten_password
(define-entrypoint forgotten_password :post
  () (user_id)
  :resource "/forgotton_password"
  :documentation "Prompt a random reset code to be emailed to a user to commence the password reset process, this code expires after 20 minutes.")

;; PATCH /resourceful/forgotten_password
(define-entrypoint forgotten_password :patch
  () (user_id new_password answers reset_code)
  :resource "/forgotten_password"
  :documentation "Given the answers to a user's secret questions, update the user's password to the new password given in this call.")
