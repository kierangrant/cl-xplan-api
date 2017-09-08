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

File: src/api/id_verification/driver_license.lisp
Description: /id_verification/driver_license API functions
|#

(in-package :cl-xplan-api/api)

;; id_verification/driver_license - GET /resourceful/id_verification/driver_license
(define-entrypoint id_verification/driver_license :get
  () (entity_id)
  :resource "/id_verification/driver_license"
  :documentation "Given the client's id, return the driver license fields for this client.")

;; id_verification/driver_license - PATCH /resourceful/id_verification/driver_license
(define-entrypoint id_verification/driver_license :patch
  () (entity_id country_of_origin driver_license_number issue_date)
  :resource "/id_verification/driver_license"
  :documentation "Given the driver license fields, update the client's driver license info.")
