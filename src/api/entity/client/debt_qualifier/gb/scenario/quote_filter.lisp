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

File: src/api/entity/client/debt_qualifier/gb/scenario/quote_filter.lisp
Description: /entity/client/debt_qualifier/gb/scenario/quote_filter API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/debt_qualifier/gb/scenario/quote_filter
				 "debt_qualifier/gb/scenario" "/entity/client" "quote_filter")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults
    (:inhibit nil
     :default-args
     (total_pay_over filter_lender max_floor_rate max_rate
      ((enhanced_panel nil enhanced_panel-p) :cond enhanced_panel-p :value (if enhanced_panel 1 0))
      ((direct_product nil direct_product-p) :cond direct_product-p :value (if direct_product 1 0))
      ((step_product nil step_product-p) :cond step_product-p :value (if step_product 1 0))
      ((refer_criteria nil refer_criteria-p) :cond refer_criteria-p :value (if refer_criteria 1 0))
      ((exclusives nil exclusives-p) :cond exclusives-p :value (if exclusives 1 0))
      ((ineligible_product nil ineligible_product-p) :cond ineligible_product-p :value (if ineligible_product 1 0))
      ((no_bank_statement nil no_bank_statement-p) :cond no_bank_statement-p :value (if no_bank_statement 1 0))
      ((no_payslips nil no_payslips-p) :cond no_payslips-p :value (if no_payslips 1 0))
      ((no_p60 nil no_p60-p) :cond no_p60-p :value (if no_p60 1 0))
      ((no_lender_refs nil no_lender_refs-p) :cond no_lender_refs-p :value (if no_lender_refs 1 0))
      ((daily nil daily-p) :cond daily-p :value (if daily 1 0))
      ((monthly nil monthly-p) :cond monthly-p :value (if monthly 1 0))
      ((quarterly nil quarterly-p) :cond quarterly-p :value (if quarterly 1 0))
      ((annually nil annually-p) :cond annually-p :value (if annually 1 0))
      ((no_floor_rate nil no_floor_rate-p) :cond no_floor_rate-p :value (if no_floor_rate 1 0)))))

(define-dynamiclike-entrypoints (entity/client-v2/debt_qualifier/gb/scenario/quote_filter
				 "debt_qualifier/gb/scenario" "/entity/client-v2" "quote_filter")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults
    (:inhibit nil
     :default-args
     (total_pay_over filter_lender max_floor_rate max_rate
      ((enhanced_panel nil enhanced_panel-p) :cond enhanced_panel-p :value (if enhanced_panel 1 0))
      ((direct_product nil direct_product-p) :cond direct_product-p :value (if direct_product 1 0))
      ((step_product nil step_product-p) :cond step_product-p :value (if step_product 1 0))
      ((refer_criteria nil refer_criteria-p) :cond refer_criteria-p :value (if refer_criteria 1 0))
      ((exclusives nil exclusives-p) :cond exclusives-p :value (if exclusives 1 0))
      ((ineligible_product nil ineligible_product-p) :cond ineligible_product-p :value (if ineligible_product 1 0))
      ((no_bank_statement nil no_bank_statement-p) :cond no_bank_statement-p :value (if no_bank_statement 1 0))
      ((no_payslips nil no_payslips-p) :cond no_payslips-p :value (if no_payslips 1 0))
      ((no_p60 nil no_p60-p) :cond no_p60-p :value (if no_p60 1 0))
      ((no_lender_refs nil no_lender_refs-p) :cond no_lender_refs-p :value (if no_lender_refs 1 0))
      ((daily nil daily-p) :cond daily-p :value (if daily 1 0))
      ((monthly nil monthly-p) :cond monthly-p :value (if monthly 1 0))
      ((quarterly nil quarterly-p) :cond quarterly-p :value (if quarterly 1 0))
      ((annually nil annually-p) :cond annually-p :value (if annually 1 0))
      ((no_floor_rate nil no_floor_rate-p) :cond no_floor_rate-p :value (if no_floor_rate 1 0)))))

(define-dynamiclike-entrypoints (entity/client-v3/debt_qualifier/gb/scenario/quote_filter
				 "debt_qualifier/gb/scenario" "/entity/client-v3" "quote_filter")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults
    (:inhibit nil
     :default-args
     (total_pay_over filter_lender max_floor_rate max_rate
      ((enhanced_panel nil enhanced_panel-p) :cond enhanced_panel-p :value (if enhanced_panel 1 0))
      ((direct_product nil direct_product-p) :cond direct_product-p :value (if direct_product 1 0))
      ((step_product nil step_product-p) :cond step_product-p :value (if step_product 1 0))
      ((refer_criteria nil refer_criteria-p) :cond refer_criteria-p :value (if refer_criteria 1 0))
      ((exclusives nil exclusives-p) :cond exclusives-p :value (if exclusives 1 0))
      ((ineligible_product nil ineligible_product-p) :cond ineligible_product-p :value (if ineligible_product 1 0))
      ((no_bank_statement nil no_bank_statement-p) :cond no_bank_statement-p :value (if no_bank_statement 1 0))
      ((no_payslips nil no_payslips-p) :cond no_payslips-p :value (if no_payslips 1 0))
      ((no_p60 nil no_p60-p) :cond no_p60-p :value (if no_p60 1 0))
      ((no_lender_refs nil no_lender_refs-p) :cond no_lender_refs-p :value (if no_lender_refs 1 0))
      ((daily nil daily-p) :cond daily-p :value (if daily 1 0))
      ((monthly nil monthly-p) :cond monthly-p :value (if monthly 1 0))
      ((quarterly nil quarterly-p) :cond quarterly-p :value (if quarterly 1 0))
      ((annually nil annually-p) :cond annually-p :value (if annually 1 0))
      ((no_floor_rate nil no_floor_rate-p) :cond no_floor_rate-p :value (if no_floor_rate 1 0)))))
