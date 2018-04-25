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

File: src/api/entity/client/debt_qualifier/au/scenario/lender_result.lisp
Description: /entity/client/debt_qualifier/au/scenario/lender_result API functions
|#

(in-package :cl-xplan-api/api)

;; entity/client/debt_qualifier/au/scenario/lender_result - GET /resourceful/entity/client/:entity_id/debt_qualifier/au/scenario/:scenario_id/lender_result
(define-entrypoint entity/client/debt_qualifier/au/scenario/lender_result :get
  (entity_id scenario_id) ()
  :resource (format nil "/entity/client/~A/debt_qualifier/au/scenario/~A/lender_result" entity_id scenario_id))

;; entity/client-v2/debt_qualifier/au/scenario/lender_result - GET /resourceful/entity/client-v2/:entity_id/debt_qualifier/au/scenario/:scenario_id/lender_result
(define-entrypoint entity/client/debt_qualifier/au/scenario/lender_result :get
  (entity_id scenario_id) ()
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/au/scenario/~A/lender_result" entity_id scenario_id))

;; entity/client-v3/debt_qualifier/au/scenario/lender_result - GET /resourceful/entity/client-v3/:entity_id/debt_qualifier/au/scenario/:scenario_id/lender_result
(define-entrypoint entity/client-v3/debt_qualifier/au/scenario/lender_result :get
  (entity_id scenario_id) ()
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/au/scenario/~A/lender_result" entity_id scenario_id))
