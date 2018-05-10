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

File: src/api/entity/client/debt_qualifier/au/scenario/proposed_loan.lisp
Description: /entity/client/debt_qualifier/au/scenario/proposed_loan API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints
    (entity/client/debt_qualifier/au/scenario/proposed_loan "debt_qualifier/au/scenario" "/entity/client"
							    "proposed_loan")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field loan_id)
    :get-defaults (:default-args nil)
    :post-defaults
    (:default-args
	(((proposed_loan_main_account nil proposed_loan_main_account-p) :cond proposed_loan_main_account-p
	  :value (if proposed_loan_main_account 1 0))
	 ((proposed_loan_investment nil proposed_loan_investment-p) :cond proposed_loan_investment-p
	  :value (if proposed_loan_investment 1 0))
	 ((proposed_loan_implemented nil proposed_loan_implemented-p) :cond proposed_loan_implemented-p
	  :value (if proposed_loan_implemented 1 0))
	 proposed_loan_type proposed_loan_amount proposed_loan_term proposed_loan_repayment_type
	 proposed_loan_io_term proposed_loan_feature))
    :patch-defaults
    (:default-args
	(((proposed_loan_main_account nil proposed_loan_main_account-p) :cond proposed_loan_main_account-p
	  :value (if proposed_loan_main_account 1 0))
	 ((proposed_loan_investment nil proposed_loan_investment-p) :cond proposed_loan_investment-p
	  :value (if proposed_loan_investment 1 0))
	 ((proposed_loan_implemented nil proposed_loan_implemented-p) :cond proposed_loan_implemented-p
	  :value (if proposed_loan_implemented 1 0))
	 proposed_loan_type proposed_loan_amount proposed_loan_term proposed_loan_repayment_type
	 proposed_loan_io_term proposed_loan_feature))
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints
    (entity/client-v2/debt_qualifier/au/scenario/proposed_loan "debt_qualifier/au/scenario" "/entity/client-v2"
							       "proposed_loan")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field loan_id)
    :get-defaults (:default-args nil)
    :post-defaults
    (:default-args
	(((proposed_loan_main_account nil proposed_loan_main_account-p) :cond proposed_loan_main_account-p
	  :value (if proposed_loan_main_account 1 0))
	 ((proposed_loan_investment nil proposed_loan_investment-p) :cond proposed_loan_investment-p
	  :value (if proposed_loan_investment 1 0))
	 ((proposed_loan_implemented nil proposed_loan_implemented-p) :cond proposed_loan_implemented-p
	  :value (if proposed_loan_implemented 1 0))
	 proposed_loan_type proposed_loan_amount proposed_loan_term proposed_loan_repayment_type
	 proposed_loan_io_term proposed_loan_feature))
    :patch-defaults
    (:default-args
	(((proposed_loan_main_account nil proposed_loan_main_account-p) :cond proposed_loan_main_account-p
	  :value (if proposed_loan_main_account 1 0))
	 ((proposed_loan_investment nil proposed_loan_investment-p) :cond proposed_loan_investment-p
	  :value (if proposed_loan_investment 1 0))
	 ((proposed_loan_implemented nil proposed_loan_implemented-p) :cond proposed_loan_implemented-p
	  :value (if proposed_loan_implemented 1 0))
	 proposed_loan_type proposed_loan_amount proposed_loan_term proposed_loan_repayment_type
	 proposed_loan_io_term proposed_loan_feature))
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints
    (entity/client-v3/debt_qualifier/au/scenario/proposed_loan "debt_qualifier/au/scenario" "/entity/client-v3"
							       "proposed_loan")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field loan_id)
    :get-defaults (:default-args nil)
    :post-defaults
    (:default-args
	(((proposed_loan_main_account nil proposed_loan_main_account-p) :cond proposed_loan_main_account-p
	  :value (if proposed_loan_main_account 1 0))
	 ((proposed_loan_investment nil proposed_loan_investment-p) :cond proposed_loan_investment-p
	  :value (if proposed_loan_investment 1 0))
	 ((proposed_loan_implemented nil proposed_loan_implemented-p) :cond proposed_loan_implemented-p
	  :value (if proposed_loan_implemented 1 0))
	 proposed_loan_type proposed_loan_amount proposed_loan_term proposed_loan_repayment_type
	 proposed_loan_io_term proposed_loan_feature))
    :patch-defaults
    (:default-args
	(((proposed_loan_main_account nil proposed_loan_main_account-p) :cond proposed_loan_main_account-p
	  :value (if proposed_loan_main_account 1 0))
	 ((proposed_loan_investment nil proposed_loan_investment-p) :cond proposed_loan_investment-p
	  :value (if proposed_loan_investment 1 0))
	 ((proposed_loan_implemented nil proposed_loan_implemented-p) :cond proposed_loan_implemented-p
	  :value (if proposed_loan_implemented 1 0))
	 proposed_loan_type proposed_loan_amount proposed_loan_term proposed_loan_repayment_type
	 proposed_loan_io_term proposed_loan_feature))
    :put-defaults (:inhibit t))
