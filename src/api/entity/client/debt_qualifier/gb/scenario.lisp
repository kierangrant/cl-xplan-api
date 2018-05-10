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

File: src/api/entity/client/debt_qualifier/gb/scenario.lisp
Description: /entity/client/debt_qualifier/gb/scenario API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints
    (entity/client/debt_qualifier/gb/scenario "debt_qualifier/gb/scenario" "/entity/client")
    :request-defaults (:list-obj-field scenario_id)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (scenario_name lender_group))
    :patch-defaults (:default-args (product_feature product_fee total_to_pay max_arrange_book_fee property_feature
						    no_bedroom no_kitchen vendor_owned_year vendor_owned_month
						    no_unit_development no_unit_purchasing lease_remaining
						    flat_type tenancy_feature no_occupants rental_income
						    expected_tenancy_year expected_tenancy_month no_households
						    discount_price broker_fee new_build buy_to_let
						    highest_income_tax_rate surplus_income_willing_to_commit
						    company_detail credit_detail scenario_applicant_detail
						    scenario_name lender_group research_liability_id quote_nature
						    ((fixed nil fixed-p) :cond fixed-p :value (if fixed 1 0))
						    ((variable nil variable-p) :cond variable-p :value (if variable 1 0))
						    ((discounted nil discounted-p) :cond discounted-p :value (if discounted 1 0))
						    ((tracker nil tracker-p) :cond tracker-p :value (if tracker 1 0))
						    ((capped nil capped-p) :cond capped-p :value (if capped 1 0))
						    ((libor nil libor-p) :cond libor-p :value (if libor 1 0))))
    :put-defaults (:inhibit t))

(define-entrypoint entity/client/debt_qualifier/gb/scenario :implement
  (entity_id scenario_id) (lender_id product_id)
  :single-method :post
  :single-parms-as-body t
  :resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A?_method=implement" entity_id scenario_id)
  :bulk-resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A" entity_id scenario_id))

(define-dynamiclike-entrypoints
    (entity/client-v2/debt_qualifier/gb/scenario "debt_qualifier/gb/scenario" "/entity/client-v2")
    :request-defaults (:list-obj-field scenario_id)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (scenario_name lender_group))
    :patch-defaults (:default-args (product_feature product_fee total_to_pay max_arrange_book_fee property_feature
						    no_bedroom no_kitchen vendor_owned_year vendor_owned_month
						    no_unit_development no_unit_purchasing lease_remaining
						    flat_type tenancy_feature no_occupants rental_income
						    expected_tenancy_year expected_tenancy_month no_households
						    discount_price broker_fee new_build buy_to_let
						    highest_income_tax_rate surplus_income_willing_to_commit
						    company_detail credit_detail scenario_applicant_detail
						    scenario_name lender_group research_liability_id quote_nature
						    ((fixed nil fixed-p) :cond fixed-p :value (if fixed 1 0))
						    ((variable nil variable-p) :cond variable-p :value (if variable 1 0))
						    ((discounted nil discounted-p) :cond discounted-p :value (if discounted 1 0))
						    ((tracker nil tracker-p) :cond tracker-p :value (if tracker 1 0))
						    ((capped nil capped-p) :cond capped-p :value (if capped 1 0))
						    ((libor nil libor-p) :cond libor-p :value (if libor 1 0))))
    :put-defaults (:inhibit t))

(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario :implement
  (entity_id scenario_id) (lender_id product_id)
  :single-method :post
  :single-parms-as-body t
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A?_method=implement" entity_id
		    scenario_id)
  :bulk-resource (format nil "/entity-v2/client/~A/debt_qualifier/gb/scenario/~A" entity_id scenario_id))

(define-dynamiclike-entrypoints
    (entity/client-v3/debt_qualifier/gb/scenario "debt_qualifier/gb/scenario" "/entity/client-v3")
    :request-defaults (:list-obj-field scenario_id)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (scenario_name lender_group))
    :patch-defaults (:default-args (product_feature product_fee total_to_pay max_arrange_book_fee property_feature
						    no_bedroom no_kitchen vendor_owned_year vendor_owned_month
						    no_unit_development no_unit_purchasing lease_remaining
						    flat_type tenancy_feature no_occupants rental_income
						    expected_tenancy_year expected_tenancy_month no_households
						    discount_price broker_fee new_build buy_to_let
						    highest_income_tax_rate surplus_income_willing_to_commit
						    company_detail credit_detail scenario_applicant_detail
						    scenario_name lender_group research_liability_id quote_nature
						    ((fixed nil fixed-p) :cond fixed-p :value (if fixed 1 0))
						    ((variable nil variable-p) :cond variable-p :value (if variable 1 0))
						    ((discounted nil discounted-p) :cond discounted-p :value (if discounted 1 0))
						    ((tracker nil tracker-p) :cond tracker-p :value (if tracker 1 0))
						    ((capped nil capped-p) :cond capped-p :value (if capped 1 0))
						    ((libor nil libor-p) :cond libor-p :value (if libor 1 0))))
    :put-defaults (:inhibit t))

(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario :implement
  (entity_id scenario_id) (lender_id product_id)
  :single-method :post
  :single-parms-as-body t
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A?_method=implement" entity_id
		    scenario_id)
  :bulk-resource (format nil "/entity-v3/client/~A/debt_qualifier/gb/scenario/~A" entity_id scenario_id))
