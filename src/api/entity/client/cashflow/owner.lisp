#|
This file is part of CL-XPLAN-API, the Lisp XPLAN API Library

Copyright (C) 2018 Kieran Grant
This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser General Public License
(http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Lisp Lesser GNU General Public License for more details.

File: src/api/entity/client/cashflow/owner.lisp
Description: /entity/client/cashflow/owner API functions
|#

(in-package :cl-xplan-api/api)

;; entity/client/cashflow/owner - GET /resourceful/entity/client/:entity_id/cashflow/:list_obj_index/owner
(define-entrypoint entity/client/cashflow/owner :GET
  (entity_id cashflow_index) ()
  :resource (format nil "/entity/client/~A/cashflow/~A/owner" entity_id cashflow_index))

;; entity/client-v2/cashflow/owner - GET /resourceful/entity/client-v2/:entity_id/cashflow/:list_obj_index/owner
(define-entrypoint entity/client-v2/cashflow/owner :GET
  (entity_id cashflow_index) ()
  :resource (format nil "/entity/client-v2/~A/cashflow/~A/owner" entity_id cashflow_index))

;; entity/client-v3/cashflow/owner - GET /resourceful/entity/client-v3/:entity_id/cashflow/:list_obj_index/owner
(define-entrypoint entity/client-v3/cashflow/owner :GET
  (entity_id cashflow_index) ()
  :resource (format nil "/entity/client-v3/~A/cashflow/~A/owner" entity_id cashflow_index))
