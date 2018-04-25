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

File: src/api/entity/client/debt_qualifier/au/scenario/repayment_offset.lisp
Description: /entity/client/debt_qualifier/au/scenario/repayment_offset API functions
|#

(in-package :cl-xplan-api/api)

;; entity/client/debt_qualifier/au/scenario/repayment_offset - GET /resourceful/entity/client/:entity_id/debt_qualifier/au/scenario/:scenario_id/repayment_offset
(define-entrypoint entity/client/debt_qualifier/au/scenario/repayment_offset :get
  (entity_id scenario_id) ()
  :documentation "These fields can be found on Lender Results page > Repayments and Offsets popup. They are one set per qualifier scenario (One-to-one mapping)."
  :resource (format nil "/entity/client/~A/debt_qualifier/au/scenario/~A/repayment_offset" entity_id scenario_id))

;; entity/client/debt_qualifier/au/scenario/repayment_offset - PATCH /resourceful/entity/client/:entity_id/debt_qualifier/au/scenario/:scenario_id/repayment_offset
(define-entrypoint entity/client/debt_qualifier/au/scenario/repayment_offset :patch
  (entity_id scenario_id)
  (ongoing_repay_freq ongoing_repay_amount ongoing_repay_commence_year yearly_repay_amount
		      yearly_repay_commence_year offset_amount
		      ((amount_is_total_repayment nil amount_is_total_repayment-p)
		       :cond amount_is_total_repayment-p :value (if amount_is_total_repayment 1 0))
		      ((yearly_is_one_off nil yearly_is_one_off-p) :cond yearly_is_one_off-p
		       :value (if yearly_is_one_off 1 0)))
  :resource (format nil "/entity/client/~A/debt_qualifier/au/scenario/~A/repayment_offset" entity_id scenario_id))

;; entity/client-v2/debt_qualifier/au/scenario/repayment_offset - GET /resourceful/entity/client-v2/:entity_id/debt_qualifier/au/scenario/:scenario_id/repayment_offset
(define-entrypoint entity/client-v2/debt_qualifier/au/scenario/repayment_offset :get
  (entity_id scenario_id) ()
  :documentation "These fields can be found on Lender Results page > Repayments and Offsets popup. They are one set per qualifier scenario (One-to-one mapping)."
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/au/scenario/~A/repayment_offset"
		    entity_id scenario_id))

;; entity/client-v2/debt_qualifier/au/scenario/repayment_offset - PATCH /resourceful/entity/client-v2/:entity_id/debt_qualifier/au/scenario/:scenario_id/repayment_offset
(define-entrypoint entity/client-v2/debt_qualifier/au/scenario/repayment_offset :patch
  (entity_id scenario_id)
  (ongoing_repay_freq ongoing_repay_amount ongoing_repay_commence_year yearly_repay_amount
		      yearly_repay_commence_year offset_amount
		      ((amount_is_total_repayment nil amount_is_total_repayment-p)
		       :cond amount_is_total_repayment-p :value (if amount_is_total_repayment 1 0))
		      ((yearly_is_one_off nil yearly_is_one_off-p) :cond yearly_is_one_off-p
		       :value (if yearly_is_one_off 1 0)))
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/au/scenario/~A/repayment_offset" entity_id
		    scenario_id))

;; entity/client-v3/debt_qualifier/au/scenario/repayment_offset - GET /resourceful/entity/client-v3/:entity_id/debt_qualifier/au/scenario/:scenario_id/repayment_offset
(define-entrypoint entity/client-v3/debt_qualifier/au/scenario/repayment_offset :get
  (entity_id scenario_id) ()
  :documentation "These fields can be found on Lender Results page > Repayments and Offsets popup. They are one set per qualifier scenario (One-to-one mapping)."
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/au/scenario/~A/repayment_offset"
		    entity_id scenario_id))

;; entity/client-v3/debt_qualifier/au/scenario/repayment_offset - PATCH /resourceful/entity/client-v3/:entity_id/debt_qualifier/au/scenario/:scenario_id/repayment_offset
(define-entrypoint entity/client-v3/debt_qualifier/au/scenario/repayment_offset :patch
  (entity_id scenario_id)
  (ongoing_repay_freq ongoing_repay_amount ongoing_repay_commence_year yearly_repay_amount
		      yearly_repay_commence_year offset_amount
		      ((amount_is_total_repayment nil amount_is_total_repayment-p)
		       :cond amount_is_total_repayment-p :value (if amount_is_total_repayment 1 0))
		      ((yearly_is_one_off nil yearly_is_one_off-p) :cond yearly_is_one_off-p
		       :value (if yearly_is_one_off 1 0)))
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/au/scenario/~A/repayment_offset" entity_id
		    scenario_id))
