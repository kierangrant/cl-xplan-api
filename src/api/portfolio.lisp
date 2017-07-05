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

File: src/api/portfolio.lisp
Description: /portfolio API Functions
|#

(in-package :cl-xplan-api/api)

;;; portfolio

;; portfolio/position - GET /resourceful/portfolio/position and GET /resourceful/portfolio/position/:position_id
(define-entrypoint portfolio/position :get
  (position_id)
  (date
   currency
   allocation_target_id
   risk_profile
   pricing_type
   risk_ratio
   gics_sector_level
   fields
   ((settled nil settled-p) :cond settled-p :value (if settled 1 0))
   ((gross_cost nil gross-p) :cond gross-p :value (if gross_cost 1 0))
   ((include_unfilled_orders nil unfilled-p) :cond unfilled-p :value (if include_unfilled_orders 1 0))
   ((proposed nil proposed-p) :cond proposed-p :value (if proposed 1 0))
   ;; not in position/:position_id
   (aggregation :cond (and (not position_id) aggregation))
   (portfolioid :cond (and (not position_id) portfolioid))
   (accountid :cond (and (not position_id) accountid))
   ((hide_zero nil zero-p) :cond (and (not position_id) zero-p) :value (if hide_zero 1 0))
   ((fum_only nil fum-p) :cond (and (not position_id) fum-p) :value (if fum_only 1 0)))
  :documentation "Retrieve a portfolio position or a collection of portfolio position."
  :resource (format nil "/portfolio/position~@[/~A~]" position_id))

;; portfolio/position - PATCH /resourceful/portfolio/position/:position_id
(define-entrypoint portfolio/position :patch
  (position_id)
  (reinvestment
   notes
   reference
   maturity_date
   name_override
   security_yield
   security_commencementdate
   custom_total_cost
   custom_total_cost_date
   cfd_provider_code
   fields
   ((cgt nil cgt-p) :cond cgt-p :value (if cgt 1 0))
   ((exclude_ipsv nil ipsv-p) :cond ipsv-p :value (if exclude_ipsv 1 0))
   ((management nil management-p) :cond management-p :value (if management 1 0))
   ((cash nil cash-p) :cond cash-p :value (if cash 1 0))
   ((is_excluded_from_fees nil exclude-fee-p) :cond exclude-fee-p :value (if is_excluded_from_fees 1 0))
   ((custom_data nil custom-p) :cond custom-p :value (if custom_data 1 ))
   ((proposed nil proposed-p) :cond proposed-p :value (if proposed 1 0)))
  :resource (format nil "/portfolio/position/~A" position_id))

;;; portfolio/profit_analysis

;; portfolio/profit_analysis/detail - GET /resourceful/portfolio/profit_analysis/detail

(define-entrypoint portfolio/profit_analysis/detail :get
  ()
  (start_date end_date grouping from_pricing_type to_pricing_type fields portfolioid accountid
	      fees_and_taxes
	      ((include_irr nil irr-p) :cond irr-p :value (if include_irr 1 0))
	      ((include_twrr nil twrr-p) :cond twrr-p :value (if include_twrr 1 0))
	      ((proposed nil proposed-p) :cond proposed-p :value (if proposed 1 0))
	      ((exclude_cash nil cash-p) :cond cash-p :value (if exclude_cash 1 0))
	      ((apply_excess_unless_fum nil excess-p) :cond excess-p :value (if apply_excess_unless_fum 1 0) :string "apply_excess_unless_FUM")
	      ((show_hidden_positions nil hidden-p) :cond hidden-p :value (if show_hidden_positions 1 0))
	      ((settled nil settled-p) :cond settled-p :value (if settled 1 0)))
  :documentation "Profit analysis report."
  :resource "/portfolio/profit_analysis/detail")

;; portfolio/transaction - GET /resourceful/portfolio/transaction/:transaction_id and GET /resourceful/portfolio/transaction
(define-entrypoint portfolio/transaction :get
  (transaction_id)
  (fields
   (start_date :cond (and (not transaction_id) start_date))
   (stop_date :cond (and (not transaction_id) stop_date))
   (page_bookmark :cond (and (not transaction_id) page_bookmark))
   (page_size :cond (and (not transaction_id) page_size))
   (sort_type :cond (and (not transaction_id) sort_type))
   (date_filter :cond (and (not transaction_id) date_filter))
   (exclude_cash :cond (and (not transaction_id) exclude_cash))
   (transaction_type :cond (and (not transaction_id) transaction_type))
   (transaction_status :cond (and (not transaction_id) transaction_status))
   (security_id :cond (and (not transaction_id) security_id))
   (vendor :cond (and (not transaction_id) vendor))
   (external_id :cond (and (not transaction_id) external_id))
   (external_id_contains :cond (and (not transaction_id) external_id_contains))
   (currency :cond (and (not transaction_id) currency))
   (portfolioid :cond (and (not transaction_id) portfolioid))
   (accountid :cond (and (not transaction_id) accountid))
   ((apply_exclude_unless_fum nil apply-exclude-p) :cond (and (not transaction_id) apply-exclude-p) :value (if apply_exclude_unless_fum 1 0) :string "apply_exclude_unless_FUM")
   ((proposed nil proposed-p) :cond proposed-p :value (if proposed 1 0))
   ((show_hidden_positions nil show-hidden-p) :cond show-hidden-p :value (if show_hidden_positions 1 0)))
  :documentation "Retrieve a transaction or a collection.
If you provide a transaction ID (as an integer) only the fields paramater is used.
If no portfolio is given, all transactions visible to the user are returned."
  :resource (format nil "/portfolio/transaction~@[/~A~]" transaction_id))

;; portfolio/transaction - PATCH /resourceful/portfolio/transaction/:transaction_id
(define-entrypoint portfolio/transaction :patch
  (transaction_id)
  (transaction_type comment trade_date tax_date settlement_date volume value price_per_unit transaction_status transaction_subtype)
  :resource (format NIL "/portfolio/transaction/~A" transaction_id))

;; portfolio/transaction - DELETE /resourceful/portfolio/transaction/:transaction_id
(define-entrypoint portfolio/transaction :delete (transaction_id) ()
		   :resource (format nil "/portfolio/transaction/~A" transaction_id))

;; portfolio/transaction_types - GET /resourceful/portfolio/transaction_types
(define-entrypoint portfolio/transaction_types :get () () :resource "/portfolio/transaction_types")
