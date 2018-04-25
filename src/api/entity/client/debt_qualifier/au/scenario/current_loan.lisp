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

File: src/api/entity/client/debt_qualifier/au/scenario/current_loan.lisp
Description: /entity/client/debt_qualifier/au/scenario/current_loan API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/debt_qualifier/au/scenario/current_loan
				 "debt_qualifier/au/scenario" "/entity/client" "current_loan")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field loan_id)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args
		       (current_loan_type current_loan_amount current_loan_term current_loan_init_rate
					  current_loan_use current_loan_repayment_type current_loan_lender
					  current_loan_init_term current_loan_revert_rate current_loan_repayment
					  current_loan_discharge_fee current_loan_def current_loan_ongoing_fee
					  current_loan_break_cost current_loan_settle_date current_loan_init_amount
					  current_loan_lmi_paid current_loan_io_term current_loan_government_paid))
    :patch-defaults (:default-args
			(current_loan_type current_loan_amount current_loan_term current_loan_init_rate
					   current_loan_use current_loan_repayment_type current_loan_lender
					   current_loan_init_term current_loan_revert_rate current_loan_repayment
					   current_loan_discharge_fee current_loan_def current_loan_ongoing_fee
					   current_loan_break_cost current_loan_settle_date
					   current_loan_init_amount current_loan_lmi_paid current_loan_io_term
					   current_loan_government_paid))
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints (entity/client-v2/debt_qualifier/au/scenario/current_loan
				 "debt_qualifier/au/scenario" "/entity/client-v2" "current_loan")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field loan_id)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args
		       (current_loan_type current_loan_amount current_loan_term current_loan_init_rate
					  current_loan_use current_loan_repayment_type current_loan_lender
					  current_loan_init_term current_loan_revert_rate current_loan_repayment
					  current_loan_discharge_fee current_loan_def current_loan_ongoing_fee
					  current_loan_break_cost current_loan_settle_date current_loan_init_amount
					  current_loan_lmi_paid current_loan_io_term current_loan_government_paid))
    :patch-defaults (:default-args
			(current_loan_type current_loan_amount current_loan_term current_loan_init_rate
					   current_loan_use current_loan_repayment_type current_loan_lender
					   current_loan_init_term current_loan_revert_rate current_loan_repayment
					   current_loan_discharge_fee current_loan_def current_loan_ongoing_fee
					   current_loan_break_cost current_loan_settle_date
					   current_loan_init_amount current_loan_lmi_paid current_loan_io_term
					   current_loan_government_paid))
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints (entity/client-v3/debt_qualifier/au/scenario/current_loan
				 "debt_qualifier/au/scenario" "/entity/client-v3" "current_loan")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field loan_id)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args
		       (current_loan_type current_loan_amount current_loan_term current_loan_init_rate
					  current_loan_use current_loan_repayment_type current_loan_lender
					  current_loan_init_term current_loan_revert_rate current_loan_repayment
					  current_loan_discharge_fee current_loan_def current_loan_ongoing_fee
					  current_loan_break_cost current_loan_settle_date current_loan_init_amount
					  current_loan_lmi_paid current_loan_io_term current_loan_government_paid))
    :patch-defaults (:default-args
			(current_loan_type current_loan_amount current_loan_term current_loan_init_rate
					   current_loan_use current_loan_repayment_type current_loan_lender
					   current_loan_init_term current_loan_revert_rate current_loan_repayment
					   current_loan_discharge_fee current_loan_def current_loan_ongoing_fee
					   current_loan_break_cost current_loan_settle_date
					   current_loan_init_amount current_loan_lmi_paid current_loan_io_term
					   current_loan_government_paid))
    :put-defaults (:inhibit t))
