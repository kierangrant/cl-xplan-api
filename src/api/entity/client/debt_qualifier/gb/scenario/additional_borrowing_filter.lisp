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

File: src/api/entity/client/debt_qualifier/gb/scenario/additional_borrowing_filter.lisp
Description: /entity/client/debt_qualifier/gb/scenario/additional_borrowing_filter API functions
|#

(in-package :cl-xplan-api/api)

;; entity/client/debt_qualifier/gb/scenario/additional_borrowing_filter - GET /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/additional_borrowing_filter
(define-entrypoint entity/client/debt_qualifier/gb/scenario/additional_borrowing_filter :get
  (entity_id scenario_id) ()
  :resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/additional_borrowing_filter"
		    entity_id scenario_id))

;; entity/client/debt_qualifier/gb/scenario/additional_borrowing_filter - PATCH /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/additional_borrowing_filter
(define-entrypoint entity/client/debt_qualifier/gb/scenario/additional_borrowing_filter :patch
  (entity_id scenario_id rate_type.scenario_id (rate_type.all_type nil rate_type.all_type-p)
	     (rate_type.fixed nil rate_type.fixed-p) (rate_type.variable nil rate_type.variable-p)
	     (rate_type.discounted nil rate_type.discounted-p) (rate_type.tracker nil rate_type.tracker-p)
	     (rate_type.capped nil rate_type.capped-p) (rate_type.libor nil rate_type.libor-p))
  (additional_borrowing_quote_filter additional_borrowing_feature additional_borrowing_fee 
   additional_borrowing_total_pay filter_lender max_floor_rate max_rate max_arrange_fee total_to_pay_years
   init_rate_term init_from_month init_to_month broker_fee term_year term_month 
   (rate_type
    :cond (or rate_type rate_type.scenario_id rate_type.all_type rate_type.fixed rate_type.variable
	      rate_type.discounted rate_type.tracker rate_type.capped rate_type.libor)
    :value
    (if rate_type rate_type
	(cond-hash
	  (rate_type.scenario_id "scenario_id")
	  (rate_type.all_type-p "all_type" (if rate_type.all_type t json:+json-false+))
	  (rate_type.fixed-p "fixed" (if rate_type.fixed t json:+json-false+))
	  (rate_type.variable-p "variable" (if rate_type.variable t json:+json-false+))
	  (rate_type.discounted-p "discounted" (if rate_type.discounted t json:+json-false+))
	  (rate_type.tracker-p "tracker" (if rate_type.tracker t json:+json-false+))
	  (rate_type.capped-p "capped" (if rate_type.capped t json:+json-false+))
	  (rate_type.libor-p "libor" (if rate_type.libor t json:+json-false+))))))
  :single-parms-as-body t
  :resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/additional_borrowing_filter"
		    entity_id scenario_id))

;; entity/client-v2/debt_qualifier/gb/scenario/additional_borrowing_filter - GET /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/additional_borrowing_filter
(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario/additional_borrowing_filter :get
  (entity_id scenario_id) ()
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/additional_borrowing_filter"
		    entity_id scenario_id))

;; entity/client-v2/debt_qualifier/gb/scenario/additional_borrowing_filter - PATCH /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/additional_borrowing_filter
(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario/additional_borrowing_filter :patch
  (entity_id scenario_id rate_type.scenario_id (rate_type.all_type nil rate_type.all_type-p)
	     (rate_type.fixed nil rate_type.fixed-p) (rate_type.variable nil rate_type.variable-p)
	     (rate_type.discounted nil rate_type.discounted-p) (rate_type.tracker nil rate_type.tracker-p)
	     (rate_type.capped nil rate_type.capped-p) (rate_type.libor nil rate_type.libor-p))
  (additional_borrowing_quote_filter additional_borrowing_feature additional_borrowing_fee 
   additional_borrowing_total_pay filter_lender max_floor_rate max_rate max_arrange_fee total_to_pay_years
   init_rate_term init_from_month init_to_month broker_fee term_year term_month 
   (rate_type
    :cond (or rate_type rate_type.scenario_id rate_type.all_type rate_type.fixed rate_type.variable
	      rate_type.discounted rate_type.tracker rate_type.capped rate_type.libor)
    :value
    (if rate_type rate_type
	(cond-hash
	  (rate_type.scenario_id "scenario_id")
	  (rate_type.all_type-p "all_type" (if rate_type.all_type t json:+json-false+))
	  (rate_type.fixed-p "fixed" (if rate_type.fixed t json:+json-false+))
	  (rate_type.variable-p "variable" (if rate_type.variable t json:+json-false+))
	  (rate_type.discounted-p "discounted" (if rate_type.discounted t json:+json-false+))
	  (rate_type.tracker-p "tracker" (if rate_type.tracker t json:+json-false+))
	  (rate_type.capped-p "capped" (if rate_type.capped t json:+json-false+))
	  (rate_type.libor-p "libor" (if rate_type.libor t json:+json-false+))))))
  :single-parms-as-body t
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/additional_borrowing_filter"
		    entity_id scenario_id))

;; entity/client-v3/debt_qualifier/gb/scenario/additional_borrowing_filter - GET /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/additional_borrowing_filter
(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario/additional_borrowing_filter :get
  (entity_id scenario_id) ()
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/additional_borrowing_filter"
		    entity_id scenario_id))

;; entity/client-v3/debt_qualifier/gb/scenario/additional_borrowing_filter - PATCH /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/additional_borrowing_filter
(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario/additional_borrowing_filter :patch
  (entity_id scenario_id rate_type.scenario_id (rate_type.all_type nil rate_type.all_type-p)
	     (rate_type.fixed nil rate_type.fixed-p) (rate_type.variable nil rate_type.variable-p)
	     (rate_type.discounted nil rate_type.discounted-p) (rate_type.tracker nil rate_type.tracker-p)
	     (rate_type.capped nil rate_type.capped-p) (rate_type.libor nil rate_type.libor-p))
  (additional_borrowing_quote_filter additional_borrowing_feature additional_borrowing_fee 
   additional_borrowing_total_pay filter_lender max_floor_rate max_rate max_arrange_fee total_to_pay_years
   init_rate_term init_from_month init_to_month broker_fee term_year term_month 
   (rate_type
    :cond (or rate_type rate_type.scenario_id rate_type.all_type rate_type.fixed rate_type.variable
	      rate_type.discounted rate_type.tracker rate_type.capped rate_type.libor)
    :value
    (if rate_type rate_type
	(cond-hash
	  (rate_type.scenario_id "scenario_id")
	  (rate_type.all_type-p "all_type" (if rate_type.all_type t json:+json-false+))
	  (rate_type.fixed-p "fixed" (if rate_type.fixed t json:+json-false+))
	  (rate_type.variable-p "variable" (if rate_type.variable t json:+json-false+))
	  (rate_type.discounted-p "discounted" (if rate_type.discounted t json:+json-false+))
	  (rate_type.tracker-p "tracker" (if rate_type.tracker t json:+json-false+))
	  (rate_type.capped-p "capped" (if rate_type.capped t json:+json-false+))
	  (rate_type.libor-p "libor" (if rate_type.libor t json:+json-false+))))))
  :single-parms-as-body t
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/additional_borrowing_filter"
		    entity_id scenario_id))
