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

File: src/api/session/user/adviser/address.lisp
Description: /session/user/adviser/address API functions
|#

(in-package :cl-xplan-api/api)

;; session/user/adviser/address - GET /resourceful/session/user/adviser/:entity_id/address
(define-entrypoint session/user/adviser/address :get
  (entity_id) (indexes fields page)
  :documentation "Provide limited access to adviser's address information (business address only)."
  :resource (format nil "/session/user/adviser/~A/address" entity_id))
