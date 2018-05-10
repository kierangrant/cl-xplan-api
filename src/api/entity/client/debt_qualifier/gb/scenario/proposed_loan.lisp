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

File: src/api/entity/client/debt_qualifier/gb/scenario/proposed_loan.lisp
Description: /entity/client/debt_qualifier/gb/scenario/proposed_loan API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/debt_qualifier/gb/scenario/proposed_loan
				 "debt_qualifier/gb/scenario" "/entity/client" "proposed_loan")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field loan_id :single-parms-as-body t)
    :get-defaults (:default-args nil)
    :post-defaults
    (:default-args
	(proposed_loan_type proposed_loan_amount proposed_loan_deposit proposed_loan_term
			    proposed_loan_init_rate_term proposed_loan_repayment_type
			    ((proposed_loan_offset nil proposed_loan_offset-p) :cond proposed_loan_offset-p
			     :value (if proposed_loan_offset 1 0))
			    proposed_loan_government_scheme proposed_loan_io_amount proposed_loan_equity_loan
			    ((proposed_loan_help_to_buy nil proposed_loan_help_to_buy-p)
			     :cond proposed_loan_help_to_buy-p :value (if proposed_loan_help_to_buy 1 0))
			    proposed_loan_share proposed_loan_stv proposed_loan_term_month proposed_loan_init_from
			    proposed_loan_init_to proposed_loan_existing_lender proposed_loan_remortgage_reason
			    proposed_loan_existing_mortgage proposed_loan_additional_borrowing))
    :patch-defaults
    (:default-args
	(proposed_loan_type proposed_loan_amount proposed_loan_deposit proposed_loan_term
			    proposed_loan_init_rate_term proposed_loan_repayment_type
			    ((proposed_loan_offset nil proposed_loan_offset-p) :cond proposed_loan_offset-p
			     :value (if proposed_loan_offset 1 0))
			    proposed_loan_government_scheme proposed_loan_io_amount proposed_loan_equity_loan
			    ((proposed_loan_help_to_buy nil proposed_loan_help_to_buy-p)
			     :cond proposed_loan_help_to_buy-p :value (if proposed_loan_help_to_buy 1 0))
			    proposed_loan_share proposed_loan_stv proposed_loan_term_month proposed_loan_init_from
			    proposed_loan_init_to proposed_loan_existing_lender proposed_loan_remortgage_reason
			    proposed_loan_existing_mortgage proposed_loan_additional_borrowing))
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints (entity/client-v2/debt_qualifier/gb/scenario/proposed_loan
				 "debt_qualifier/gb/scenario" "/entity/client-v2" "proposed_loan")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field loan_id :single-parms-as-body t)
    :get-defaults (:default-args nil)
    :post-defaults
    (:default-args
	(proposed_loan_type proposed_loan_amount proposed_loan_deposit proposed_loan_term
			    proposed_loan_init_rate_term proposed_loan_repayment_type
			    ((proposed_loan_offset nil proposed_loan_offset-p) :cond proposed_loan_offset-p
			     :value (if proposed_loan_offset 1 0))
			    proposed_loan_government_scheme proposed_loan_io_amount proposed_loan_equity_loan
			    ((proposed_loan_help_to_buy nil proposed_loan_help_to_buy-p)
			     :cond proposed_loan_help_to_buy-p :value (if proposed_loan_help_to_buy 1 0))
			    proposed_loan_share proposed_loan_stv proposed_loan_term_month proposed_loan_init_from
			    proposed_loan_init_to proposed_loan_existing_lender proposed_loan_remortgage_reason
			    proposed_loan_existing_mortgage proposed_loan_additional_borrowing))
    :patch-defaults
    (:default-args
	(proposed_loan_type proposed_loan_amount proposed_loan_deposit proposed_loan_term
			    proposed_loan_init_rate_term proposed_loan_repayment_type
			    ((proposed_loan_offset nil proposed_loan_offset-p) :cond proposed_loan_offset-p
			     :value (if proposed_loan_offset 1 0))
			    proposed_loan_government_scheme proposed_loan_io_amount proposed_loan_equity_loan
			    ((proposed_loan_help_to_buy nil proposed_loan_help_to_buy-p)
			     :cond proposed_loan_help_to_buy-p :value (if proposed_loan_help_to_buy 1 0))
			    proposed_loan_share proposed_loan_stv proposed_loan_term_month proposed_loan_init_from
			    proposed_loan_init_to proposed_loan_existing_lender proposed_loan_remortgage_reason
			    proposed_loan_existing_mortgage proposed_loan_additional_borrowing))
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints (entity/client-v3/debt_qualifier/gb/scenario/proposed_loan
				 "debt_qualifier/gb/scenario" "/entity/client-v3" "proposed_loan")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field loan_id :single-parms-as-body t)
    :get-defaults (:default-args nil)
    :post-defaults
    (:default-args
	(proposed_loan_type proposed_loan_amount proposed_loan_deposit proposed_loan_term
			    proposed_loan_init_rate_term proposed_loan_repayment_type
			    ((proposed_loan_offset nil proposed_loan_offset-p) :cond proposed_loan_offset-p
			     :value (if proposed_loan_offset 1 0))
			    proposed_loan_government_scheme proposed_loan_io_amount proposed_loan_equity_loan
			    ((proposed_loan_help_to_buy nil proposed_loan_help_to_buy-p)
			     :cond proposed_loan_help_to_buy-p :value (if proposed_loan_help_to_buy 1 0))
			    proposed_loan_share proposed_loan_stv proposed_loan_term_month proposed_loan_init_from
			    proposed_loan_init_to proposed_loan_existing_lender proposed_loan_remortgage_reason
			    proposed_loan_existing_mortgage proposed_loan_additional_borrowing))
    :patch-defaults
    (:default-args
	(proposed_loan_type proposed_loan_amount proposed_loan_deposit proposed_loan_term
			    proposed_loan_init_rate_term proposed_loan_repayment_type
			    ((proposed_loan_offset nil proposed_loan_offset-p) :cond proposed_loan_offset-p
			     :value (if proposed_loan_offset 1 0))
			    proposed_loan_government_scheme proposed_loan_io_amount proposed_loan_equity_loan
			    ((proposed_loan_help_to_buy nil proposed_loan_help_to_buy-p)
			     :cond proposed_loan_help_to_buy-p :value (if proposed_loan_help_to_buy 1 0))
			    proposed_loan_share proposed_loan_stv proposed_loan_term_month proposed_loan_init_from
			    proposed_loan_init_to proposed_loan_existing_lender proposed_loan_remortgage_reason
			    proposed_loan_existing_mortgage proposed_loan_additional_borrowing))
    :put-defaults (:inhibit t))
