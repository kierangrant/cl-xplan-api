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

File: src/api/entity/client/debt_qualifier/gb/scenario/selected_product.lisp
Description: /entity/client/debt_qualifier/gb/scenario/selected_product API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/debt_qualifier/gb/scenario/selected_product
				 "debt_qualifier/gb/scenario" "/entity/client" "selected_product")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field product_id :single-parms-as-body t)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (product_id lender_name lender_id product_desc init_rate apr repayment
					      product_type revert_rate total_to_pay total_to_pay_over_term
					      recommended_note chosen_note broker
					      ((recommended nil recommended-p) :cond recommended-p
					       :value (if recommended 1 0))
					      ((chosen nil chosen-p) :cond chosen-p :value (if chosen 1 0))
					      lender_logo_small lender_logo_large request_view lender_percentage
					      product_rates))
    :patch-defaults (:default-args (((affordability_review nil affordability_review-p)
				     :cond affordability_review-p :value (if affordability_review 1 0))
				    ((kfi_produced nil kfi_produced-p) :cond kfi_produced-p
				     :value (if kfi_produced 1 0))
				    ((suitability_letter_produced nil suitability_letter_produced-p)
				     :cond suitability_letter_produced-p
				     :value (if suitability_letter_produced 1 0))
				    ((application_submitted nil application_submitted-p)
				     :cond application_submitted-p :value (if application_submitted 1 0))
				    recommended_note chosen_note broker
				    ((recommended nil recommended-p) :cond recommended-p
				     :value (if recommended 1 0))
				    ((chosen nil chosen-p) :cond chosen-p :value (if chosen 1 0))
				    lender_percentage)))

;; entity/client/debt_qualifier/gb/scenario/selected_product - POST /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/selected_product/:product_id?_method=apply
(define-entrypoint entity/client/debt_qualifier/gb/scenario/selected_product :apply
  (entity_id scenario_id product_id) (ownership_type application_type service_type)
  :documentation "Return the LenderHub Apply url"
  :single-method :post
  :single-parms-as-body t
  :single-resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/selected_product/~A?_method=apply"
			   entity_id scenario_id product_id)
  :resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/selected_product/~A"
		    entity_id scenario_id product_id))

(define-dynamiclike-entrypoints (entity/client-v2/debt_qualifier/gb/scenario/selected_product
				 "debt_qualifier/gb/scenario" "/entity/client-v2" "selected_product")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field product_id :single-parms-as-body t)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (product_id lender_name lender_id product_desc init_rate apr repayment
					      product_type revert_rate total_to_pay total_to_pay_over_term
					      recommended_note chosen_note broker
					      ((recommended nil recommended-p) :cond recommended-p
					       :value (if recommended 1 0))
					      ((chosen nil chosen-p) :cond chosen-p :value (if chosen 1 0))
					      lender_logo_small lender_logo_large request_view lender_percentage
					      product_rates))
    :patch-defaults (:default-args (((affordability_review nil affordability_review-p)
				     :cond affordability_review-p :value (if affordability_review 1 0))
				    ((kfi_produced nil kfi_produced-p) :cond kfi_produced-p
				     :value (if kfi_produced 1 0))
				    ((suitability_letter_produced nil suitability_letter_produced-p)
				     :cond suitability_letter_produced-p
				     :value (if suitability_letter_produced 1 0))
				    ((application_submitted nil application_submitted-p)
				     :cond application_submitted-p :value (if application_submitted 1 0))
				    recommended_note chosen_note broker
				    ((recommended nil recommended-p) :cond recommended-p
				     :value (if recommended 1 0))
				    ((chosen nil chosen-p) :cond chosen-p :value (if chosen 1 0))
				    lender_percentage)))

;; entity/client-v2/debt_qualifier/gb/scenario/selected_product - POST /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/selected_product/:product_id?_method=apply
(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario/selected_product :apply
  (entity_id scenario_id product_id) (ownership_type application_type service_type)
  :documentation "Return the LenderHub Apply url"
  :single-method :post
  :single-parms-as-body t
  :single-resource
  (format nil "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/selected_product/~A?_method=apply"
	  entity_id scenario_id product_id)
  :resource (format nil "/entity/client-2/~A/debt_qualifier/gb/scenario/~A/selected_product/~A"
		    entity_id scenario_id product_id))

(define-dynamiclike-entrypoints (entity/client-v3/debt_qualifier/gb/scenario/selected_product
				 "debt_qualifier/gb/scenario" "/entity/client-v3" "selected_product")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field product_id :single-parms-as-body t)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (product_id lender_name lender_id product_desc init_rate apr repayment
					      product_type revert_rate total_to_pay total_to_pay_over_term
					      recommended_note chosen_note broker
					      ((recommended nil recommended-p) :cond recommended-p
					       :value (if recommended 1 0))
					      ((chosen nil chosen-p) :cond chosen-p :value (if chosen 1 0))
					      lender_logo_small lender_logo_large request_view lender_percentage
					      product_rates))
    :patch-defaults (:default-args (((affordability_review nil affordability_review-p)
				     :cond affordability_review-p :value (if affordability_review 1 0))
				    ((kfi_produced nil kfi_produced-p) :cond kfi_produced-p
				     :value (if kfi_produced 1 0))
				    ((suitability_letter_produced nil suitability_letter_produced-p)
				     :cond suitability_letter_produced-p
				     :value (if suitability_letter_produced 1 0))
				    ((application_submitted nil application_submitted-p)
				     :cond application_submitted-p :value (if application_submitted 1 0))
				    recommended_note chosen_note broker
				    ((recommended nil recommended-p) :cond recommended-p
				     :value (if recommended 1 0))
				    ((chosen nil chosen-p) :cond chosen-p :value (if chosen 1 0))
				    lender_percentage)))

;; entity/client-v3/debt_qualifier/gb/scenario/selected_product - POST /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/selected_product/:product_id?_method=apply
(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario/selected_product :apply
  (entity_id scenario_id product_id) (ownership_type application_type service_type)
  :documentation "Return the LenderHub Apply url"
  :single-method :post
  :single-parms-as-body t
  :single-resource
  (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/selected_product/~A?_method=apply"
	  entity_id scenario_id product_id)
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/selected_product/~A"
		    entity_id scenario_id product_id))
