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

File: src/api/session/user/adviser/contact.lisp
Description: /session/user/adviser/contact API functions
|#

(in-package :cl-xplan-api/api)

;; session/user/adviser/contact - GET /resourceful/session/user/adviser/:entity_id/contact
(define-entrypoint session/user/adviser/contact :get
  (entity_id) (indexes fields page)
  :documentation "Provide limited access to adviser's contact information (business contact only)."
  :resource (format nil "/session/user/adviser/~A/contact" entity_id))
