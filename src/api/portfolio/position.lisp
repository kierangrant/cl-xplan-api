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

File: src/api/portfolio/position.lisp
Description: /portfolio/position API Functions
|#

(in-package :cl-xplan-api/api)

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
