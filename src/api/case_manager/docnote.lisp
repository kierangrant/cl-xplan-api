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

File: src/api/case_manager/docnote.lisp
Description: /case_manager/docnote API Functions
|#

(in-package :cl-xplan-api/api)

;; case_manager/docnote - GET /resourceful/case_manager/:container_id/docnote
(define-entrypoint case_manager/docnote :GET (container_id) ()
		   :resource (format nil "/case_manager/~A/docnote" container_id))

;; case_manager/docnote - POST /resourceful/case_manager/:container_id/docnote
(define-entrypoint case_manager/docnote :post
  (container_id)
  (linked_obj_id
   ((benchmarkable nil benchmarkable-p) :cond benchmarkable-p :value (if benchmarkable 1 0)))
  :resource (format NIL "case_manager/~A/docnote" container_id))

;; case_manager/docnote - DELETE /resourceful/case_manager/:container_id/docnote/:linked_obj_id
(define-entrypoint case_manager/docnote :delete (container_id linked_obj_id) ()
		   :resource (format NIL "/case_manager/~A/docnote/~A" container_id linked_obj_id))
