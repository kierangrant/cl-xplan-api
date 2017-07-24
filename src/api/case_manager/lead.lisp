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

File: src/api/case_manager.lisp
Description: /case_manager API Functions
|#

(in-package :cl-xplan-api/api)

;; case_manager/lead - GET /resourceful/case_manager/:container_id/lead
(define-entrypoint case_manager/lead :get (container_id) ()
		   :resource (format NIL "/case_manager/~A/lead" container_id))

;; case_manager/lead - POST /resourceful/case_manager/:container_id/lead
(define-entrypoint case_manager/lead :post (container_id) (lead_id)
		   :resource (format NIL "/case_manager/~A/lead" container_id))

;; case_manager/lead - DELETE /resourceful/case_manager/:container_id/lead/:lead_id
(define-entrypoint case_manager/lead :delete (container_id lead_id) ()
		   :resource (format NIL "/case_manager/~A/lead/~A" container_id lead_id))
