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

File: src/api/case_manager/task.lisp
Description: /case_manager/task API Functions
|#

(in-package :cl-xplan-api/api)

;;; case_manager/task

;; case_manager/task - GET /resourceful/case_manager/:container_id/task
(define-entrypoint case_manager/task :get (container_id) ()
		   :resource (format nil "/case_manager/~A/task" container_id))

;; case_manager/task - POST /resourceful/case_manager/:container_id/task
(define-entrypoint case_manager/task :post
  (container_id)
  (linked_obj_id
   ((benchmarkable nil benchmarkable-p) :cond benchmarkable-p :value (if benchmarkable 1 0)))
  :resource (format nil "/case_manager/~A/task" container_id))

;; case_manager/task - DELETE /resourceful/case_manager/:container_id/task/:linked_obj_id
(define-entrypoint case_manager/task :delete (container_id linked_obj_id) ()
		   :resource (format nil "/case_manager/~A/task/~A" container_id linked_obj_id))
