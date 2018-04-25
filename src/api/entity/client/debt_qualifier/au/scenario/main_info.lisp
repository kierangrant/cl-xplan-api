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

File: src/api/entity/client/debt_qualifier/au/scenario/main_info.lisp
Description: /entity/client/debt_qualifier/au/scenario/main_info API functions
|#

(in-package :cl-xplan-api/api)

;; entity/client/debt_qualifier/au/scenario/main_info - GET /resourceful/entity/client/:entity_id/debt_qualifier/au/scenario/:scenario_id/main_info/:main_info_id
(define-entrypoint entity/client/debt_qualifier/au/scenario/main_info :get
  (entity_id scenario_id main_info_id) ()
  :resource (format nil "/entity/client/~A/debt_qualifier/au/scenario/~A/main_info/~A" entity_id scenario_id
		    main_info_id))

;; entity/client/debt_qualifier/au/scenario/main_info - PATCH /resourceful/entity/client/:entity_id/debt_qualifier/au/scenario/:scenario_id/main_info/:main_info_id
(define-entrypoint entity/client/debt_qualifier/au/scenario/main_info :patch
  (entity_id scenario_id main_info_id)
  (aapr_term lmi_type deposit_type document_level num_dependents cc_limit
	     ((paid_in_full nil paid_in_full-p) :cond paid_in_full-p :value (if paid_in_full 1 0))
	     ((first_home_buyer nil first_home_buyer-p) :cond first_home_buyer-p :value (if first_home_buyer 1 0))
	     funds_available living_expense conveyancing_fee other_loan_repayments other_liability_repayments
	     ((add_lmi nil add_lmi-p) :cond add_lmi-p :value (if add_lmi 1 0))
	     rent other_costs)
  :resource (format nil "/entity/client/~A/debt_qualifier/au/scenario/~A/main_info/~A" entity_id scenario_id
		    main_info_id))

;; entity/client-v2/debt_qualifier/au/scenario/main_info - GET /resourceful/entity/client-v2/:entity_id/debt_qualifier/au/scenario/:scenario_id/main_info/:main_info_id
(define-entrypoint entity/client-v2/debt_qualifier/au/scenario/main_info :get
  (entity_id scenario_id main_info_id) ()
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/au/scenario/~A/main_info/~A" entity_id scenario_id
		    main_info_id))

;; entity/client-v2/debt_qualifier/au/scenario/main_info - PATCH /resourceful/entity/client-v2/:entity_id/debt_qualifier/au/scenario/:scenario_id/main_info/:main_info_id
(define-entrypoint entity/client-v2/debt_qualifier/au/scenario/main_info :patch
  (entity_id scenario_id main_info_id)
  (aapr_term lmi_type deposit_type document_level num_dependents cc_limit
	     ((paid_in_full nil paid_in_full-p) :cond paid_in_full-p :value (if paid_in_full 1 0))
	     ((first_home_buyer nil first_home_buyer-p) :cond first_home_buyer-p :value (if first_home_buyer 1 0))
	     funds_available living_expense conveyancing_fee other_loan_repayments other_liability_repayments
	     ((add_lmi nil add_lmi-p) :cond add_lmi-p :value (if add_lmi 1 0))
	     rent other_costs)
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/au/scenario/~A/main_info/~A" entity_id scenario_id
		    main_info_id))

;; entity/client-v3/debt_qualifier/au/scenario/main_info - GET /resourceful/entity/client-v3/:entity_id/debt_qualifier/au/scenario/:scenario_id/main_info/:main_info_id
(define-entrypoint entity/client-v3/debt_qualifier/au/scenario/main_info :get
  (entity_id scenario_id main_info_id) ()
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/au/scenario/~A/main_info/~A" entity_id scenario_id
		    main_info_id))

;; entity/client-v3/debt_qualifier/au/scenario/main_info - PATCH /resourceful/entity/client-v3/:entity_id/debt_qualifier/au/scenario/:scenario_id/main_info/:main_info_id
(define-entrypoint entity/client-v3/debt_qualifier/au/scenario/main_info :patch
  (entity_id scenario_id main_info_id)
  (aapr_term lmi_type deposit_type document_level num_dependents cc_limit
	     ((paid_in_full nil paid_in_full-p) :cond paid_in_full-p :value (if paid_in_full 1 0))
	     ((first_home_buyer nil first_home_buyer-p) :cond first_home_buyer-p :value (if first_home_buyer 1 0))
	     funds_available living_expense conveyancing_fee other_loan_repayments other_liability_repayments
	     ((add_lmi nil add_lmi-p) :cond add_lmi-p :value (if add_lmi 1 0))
	     rent other_costs)
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/au/scenario/~A/main_info/~A" entity_id scenario_id
		    main_info_id))
