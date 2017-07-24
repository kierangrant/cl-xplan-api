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

File: src/api/case_manager/objective.lisp
Description: /case_manager/objective API Functions
|#

(in-package :cl-xplan-api/api)

;; case_manager/objective - GET /resourceful/case_manager/:container_id/objective
(define-entrypoint case_manager/objective :get (container_id) (sort_order)
		   :resource (format NIL "/case_manager/~A/objective" container_id))

;; case_manager/objective - POST /resourceful/case_manager/:container_id/objective
(define-entrypoint case_manager/objective :post (container_id) (objective_id entity_id)
		   :resource (format nil "/case_manager/~A/objective" container_id))

;; case_manager/objective - POST /resourceful/case_manager/:container_id/objective?_method=sort
(define-entrypoint case_manager/objective :sort
  (container_id) (objectives)
  :single-method :POST
  :single-resource (format nil "/case_manager/~A/objective/?_method=sort" container_id)
  :single-parms-as-body T
  :bulk-resource (format NIL "/case_manager/~A/objective" container_id))

;; case_manager/objective - DELETE /resourceful/case_manager/:container_id/objective/:objective_id
(define-entrypoint case_manager/objective :delete (container_id objective_id) (entity_id)
		   :resource (format NIL "/case_manager/~A/objective/~A" container_id objective_id))
