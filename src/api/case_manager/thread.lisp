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

File: src/api/case_manager/thread.lisp
Description: /case_manager/thread API Functions
|#

(in-package :cl-xplan-api/api)

;; case_manager/thread - GET /resourceful/case_manager/:container_id/thread
(define-entrypoint case_manager/thread :get (container_id) ()
		   :resource (format nil "/case_manager/~A/thread" container_id))

;; case_manager/thread - POST /resourceful/case_manager/:container_id/thread
(define-entrypoint case_manager/threasd :post (container_id) (thread_id)
		   :resource (format nil "/case_manager/~A/thread" container_id))

;; case_manager/thread - DELETE /resourceful/case_manager/:container_id/thread/:thread_id
(define-entrypoint case_manager/thread :delete (container_id thread_id) ()
		   :resource (format nil "/case_manager/~A/thread/~A" container_id thread_id))
