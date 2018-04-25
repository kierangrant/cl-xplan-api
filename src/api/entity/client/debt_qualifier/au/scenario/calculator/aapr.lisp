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

File: src/api/entity/client/debt_qualifier/au/scenario/calculator/aapr.lisp
Description: /entity/client/debt_qualifier/au/scenario/calculator/aapr API functions
|#

(in-package :cl-xplan-api/api)

;; entity/client/debt_qualifier/au/scenario/calculator/aapr - GET /resourceful/entity/client/:entity_id/debt_qualifier/au/scenario/:scenario_id/calculator/aapr

(define-entrypoint entity/client/debt_qualifier/au/scenario/calculator/aapr :get
  (entity_id scenario_id) (package_item_id product_item_id proposed_loan_number)
  :documentation "Resource for retriving AAPR Calculator results from LoanServer"
  :resource (format nil "/entity/client/~A/debt_qualifier/au/scenario/~A/calculator/aapr"
		    entity_id scenario_id))

;; entity/client-v2/debt_qualifier/au/scenario/calculator/aapr - GET /resourceful/entity/client-v2/:entity_id/debt_qualifier/au/scenario/:scenario_id/calculator/aapr

(define-entrypoint entity/client-v2/debt_qualifier/au/scenario/calculator/aapr :get
  (entity_id scenario_id) (package_item_id product_item_id proposed_loan_number)
  :documentation "Resource for retriving AAPR Calculator results from LoanServer"
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/au/scenario/~A/calculator/aapr"
		    entity_id scenario_id))

;; entity/client-v3/debt_qualifier/au/scenario/calculator/aapr - GET /resourceful/entity/client-v3/:entity_id/debt_qualifier/au/scenario/:scenario_id/calculator/aapr

(define-entrypoint entity/client-v3/debt_qualifier/au/scenario/calculator/aapr :get
  (entity_id scenario_id) (package_item_id product_item_id proposed_loan_number)
  :documentation "Resource for retriving AAPR Calculator results from LoanServer"
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/au/scenario/~A/calculator/aapr"
		    entity_id scenario_id))
