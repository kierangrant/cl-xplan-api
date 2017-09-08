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

File: src/api/lead/case_manager.lisp
Description: /lead/case_manager API functions
|#

(in-package :cl-xplan-api/api)

;; lead/case_manager - GET /resourceful/lead/:lead_id/case_manager
(define-entrypoint lead/case_manager :get
  (lead_in) ()
  :documentation "Collection of containers linked to this lead"
  :resource (format nil "/lead/~A/case_manager" lead_in))

;; lead/case_manager - POST /resourceful/lead/:lead_id/case_manager
(define-entrypoint lead/case_manager :post
  (lead_in) (case_manager_id)
  :resource (format nil "/lead/~A/case_manager" lead_in))
