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

File: src/api/entity/client/debt_qualifier/au/scenario/lender_maxloan_report.lisp
Description: /entity/client/debt_qualifier/au/scenario/lender_maxloan_report API functions
|#

(in-package :cl-xplan-api/api)

;; entity/client/debt_qualifier/au/scenario/lender_maxloan_report - GET /resourceful/entity/client/:entity_id/debt_qualifier/au/scenario/:scenario_id/lender_maxloan_report
(define-entrypoint entity/client/debt_qualifier/au/scenario/lender_maxloan_report :get
  (entity_id scenario_id) (lender_id
			   ((download_only nil download_only-p) :cond download_only-p
			    :value (if download_only 1 0)))
  :resource (format nil "/entity/client/~A/debt_qualifier/au/scenario/~A/lender_maxloan_report" entity_id
		    scenario_id))

;; entity/client/debt_qualifier/au/scenario/lender_maxloan_report - POST /resourceful/entity/client/:entity_id/debt_qualifier/au/scenario/:scenario_id/lender_maxloan_report?_method=abstractread
(define-entrypoint entity/client/debt_qualifier/au/scenario/lender_maxloan_report :abstractread
  (entity_id scenario_id) (((download_only nil download_only-p) :cond download_only-p
			    :value (if download_only 1 0)))
  :single-method :post
  :single-resource (format nil "/entity/client/~A/debt_qualifier/au/scenario/~A/lender_maxloan_report?_method=abstractread" entity_id scenario_id)
  :bulk-method :abstractread
  :bulk-resource (format nil "/entity/client/~A/debt_qualifier/au/scenario/~A/lender_maxloan_report" entity_id scenario_id))

;; entity/client-v2/debt_qualifier/au/scenario/lender_maxloan_report - GET /resourceful/entity/client-v2/:entity_id/debt_qualifier/au/scenario/:scenario_id/lender_maxloan_report
(define-entrypoint entity/client-v2/debt_qualifier/au/scenario/lender_maxloan_report :get
  (entity_id scenario_id) (lender_id
			   ((download_only nil download_only-p) :cond download_only-p
			    :value (if download_only 1 0)))
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/au/scenario/~A/lender_maxloan_report" entity_id
		    scenario_id))

;; entity/client-v2/debt_qualifier/au/scenario/lender_maxloan_report - POST /resourceful/entity/client-v2/:entity_id/debt_qualifier/au/scenario/:scenario_id/lender_maxloan_report?_method=abstractread
(define-entrypoint entity/client-v2/debt_qualifier/au/scenario/lender_maxloan_report :abstractread
  (entity_id scenario_id) (((download_only nil download_only-p) :cond download_only-p
			    :value (if download_only 1 0)))
  :single-method :post
  :single-resource (format nil "/entity/client-v2/~A/debt_qualifier/au/scenario/~A/lender_maxloan_report?_method=abstractread" entity_id scenario_id)
  :bulk-method :abstractread
  :bulk-resource (format nil "/entity/client-v2/~A/debt_qualifier/au/scenario/~A/lender_maxloan_report" entity_id scenario_id))

;; entity/client-v3/debt_qualifier/au/scenario/lender_maxloan_report - GET /resourceful/entity/client-v3/:entity_id/debt_qualifier/au/scenario/:scenario_id/lender_maxloan_report
(define-entrypoint entity/client-v3/debt_qualifier/au/scenario/lender_maxloan_report :get
  (entity_id scenario_id) (lender_id
			   ((download_only nil download_only-p) :cond download_only-p
			    :value (if download_only 1 0)))
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/au/scenario/~A/lender_maxloan_report" entity_id
		    scenario_id))

;; entity/client-v3/debt_qualifier/au/scenario/lender_maxloan_report - POST /resourceful/entity/client-v3/:entity_id/debt_qualifier/au/scenario/:scenario_id/lender_maxloan_report?_method=abstractread
(define-entrypoint entity/client-v3/debt_qualifier/au/scenario/lender_maxloan_report :abstractread
  (entity_id scenario_id) (((download_only nil download_only-p) :cond download_only-p
			    :value (if download_only 1 0)))
  :single-method :post
  :single-resource (format nil "/entity/client-v3/~A/debt_qualifier/au/scenario/~A/lender_maxloan_report?_method=abstractread" entity_id scenario_id)
  :bulk-method :abstractread
  :bulk-resource (format nil "/entity/client-v3/~A/debt_qualifier/au/scenario/~A/lender_maxloan_report" entity_id scenario_id))
