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

File: src/api/id_verification/passport.lisp
Description: /id_verification/passport API functions
|#

(in-package :cl-xplan-api/api)

;; id_verification/passport - GET /resourceful/id_verification/passport
(define-entrypoint id_verification/passport :get
  () (entity_id)
  :resource "/id_verification/passport"
  :documentation "Given the client's id, return the passport fields for this client.")

;; id_verification/passport - PATCH /resourceful/id_verification/passport
(define-entrypoint id_verification/passport :patch
  () (entity_id country_of_origin passport_number expiry_date)
  :resource "/id_verification/passport"
  :documentation "Given the passport fields, update the client's passport info.")
