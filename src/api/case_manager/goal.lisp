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

File: src/api/case_manager/goal.lisp
Description: /case_manager/goal API Functions
|#

(in-package :cl-xplan-api/api)

;; case_manager/goal - GET /resourceful/case_manager/:container_id/goal
(define-entrypoint case_manager/goal :get (container_id) (sort_order)
		   :resource (format NIL "/case_manager/~A/goal" container_id))

;; case_manager/goal - POST /resourceful/case_manager/:container_id/goal
(define-entrypoint case_manager/goal :post (container_id) (goal_id entity_id)
		   :resource (format NIL "/case_manager/~A/goal" container_id))

;; case_manager/goal - POST /resourceful/case_manager/:container_id/goal?_method=sort - Unsure if in Bulk I can use 'SORT' method...
(define-entrypoint case_manager/goal :sort
  (container_id) (goals)
  :single-method :POST
  :single-resource (format NIL "/case_manager/~A/goal?_method=sort" container_id)
  :single-parms-as-body T
  :bulk-resource (format NIL "/case_manager/~A/goal" container_id))

;; case_manager/goal - DELETE /resourceful/case_manager/:container_id/goal/:goal_id
(define-entrypoint case_manager/goal :delete (container_id goal_id) (entity_id)
		   :resource (format NIL "/case_manager/~A/goal/~A" container_id goal_id))
