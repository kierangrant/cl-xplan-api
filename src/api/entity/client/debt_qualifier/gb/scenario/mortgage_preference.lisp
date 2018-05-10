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

File: src/api/entity/client/debt_qualifier/gb/scenario/mortgage_preference.lisp
Description: /entity/client/debt_qualifier/gb/scenario/mortgage_preference API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/debt_qualifier/gb/scenario/mortgage_preference
				 "debt_qualifier/gb/scenario" "/entity/client" "mortgage_preference")
    :request-defaults (:list-obj-field scenario_id :inhibit-subitemid t :single-parms-as-body t)
    :get-defaults (:default-args nil)
    :post-defaults
    (:default-args
	(fixing_payment_note paying_upper_limit_note no_tie_in_note avoid_stepped_note low_init_fee_note
	 add_fee_note free_legal_fee_note refund_fee_note no_higher_lending_charge_note cash_back_note
	 over_payment_note under_payment_note payment_holiday_note offset_note borrow_back_note
	 speed_completion_note portable_note flexible_insurance_note
	 ((fixing_payment_init nil fixing_payment_init-p) :cond fixing_payment_init-p
	  :value (if fixing_payment_init 1 0))
	 ((fixing_payment_agreed nil fixing_payment_agreed-p) :cond fixing_payment_agreed-p
	  :value (if fixing_payment_agreed 1 0))
	 ((paying_upper_limit_init nil paying_upper_limit_init-p) :cond paying_upper_limit_init-p
	  :value (if paying_upper_limit_init 1 0))
	 ((paying_upper_limit_agreed nil paying_upper_limit_agreed-p) :cond paying_upper_limit_agreed-p
	  :value (if paying_upper_limit_agreed 1 0))
	 ((no_tie_in_init nil no_tie_in_init-p) :cond no_tie_in_init-p :value (if no_tie_in_init 1 0))
	 ((no_tie_in_agreed nil no_tie_in_agreed-p) :cond no_tie_in_agreed-p :value (if no_tie_in_agreed 1 0))
	 ((avoid_stepped_init nil avoid_stepped_init-p) :cond avoid_stepped_init-p
	  :value (if avoid_stepped_init 1 0))
	 ((avoid_stepped_agreed nil avoid_stepped_agreed-p) :cond avoid_stepped_agreed-p
	  :value (if avoid_stepped_agreed 1 0))
	 ((low_init_fee_init nil low_init_fee_init-p) :cond low_init_fee_init-p :value (if low_init_fee_init 1 0))
	 ((low_init_fee_agreed nil low_init_fee_agreed-p) :cond low_init_fee_agreed-p
	  :value (if low_init_fee_agreed 1 0))
	 ((add_fee_init nil add_fee_init-p) :cond add_fee_init-p :value (if add_fee_init 1 0))
	 ((add_fee_agreed nil add_fee_agreed-p) :cond add_fee_agreed-p :value (if add_fee_agreed 1 0))
	 ((free_legal_fee_init nil free_legal_fee_init-p) :cond free_legal_fee_init-p
	  :value (if free_legal_fee_init 1 0))
	 ((free_legal_fee_agreed nil free_legal_fee_agreed-p) :cond free_legal_fee_agreed-p
	  :value (if free_legal_fee_agreed 1 0))
	 ((refund_fee_init nil refund_fee_init-p) :cond refund_fee_init-p :value (if refund_fee_init 1 0))
	 ((refund_fee_agreed nil refund_fee_agreed-p) :cond refund_fee_agreed-p :value (if refund_fee_agreed 1 0))
	 ((no_higher_lending_charge_init nil no_higher_lending_charge_init-p) :cond no_higher_lending_charge_init-p
	  :value (if no_higher_lending_charge_init 1 0))
	 ((no_higher_lending_charge_agreed nil no_higher_lending_charge_agreed-p)
	  :cond no_higher_lending_charge_agreed-p :value (if no_higher_lending_charge_agreed 1 0))
	 ((cash_back_init nil cash_back_init-p) :cond cash_back_init-p :value (if cash_back_init 1 0))
	 ((cash_back_agreed nil cash_back_agreed-p) :cond cash_back_agreed-p :value (if cash_back_agreed 1 0))
	 ((over_payment_init nil over_payment_init-p) :cond over_payment_init-p :value (if over_payment_init 1 0))
	 ((over_payment_agreed nil over_payment_agreed-p) :cond over_payment_agreed-p
	  :value (if over_payment_agreed 1 0))
	 ((under_payment_init nil under_payment_init-p) :cond under_payment_init-p
	  :value (if under_payment_init 1 0))
	 ((under_payment_agreed nil under_payment_agreed-p) :cond under_payment_agreed-p
	  :value (if under_payment_agreed 1 0))
	 ((payment_holiday_init nil payment_holiday_init-p) :cond payment_holiday_init-p
	  :value (if payment_holiday_init 1 0))
	 ((payment_holiday_agreed nil payment_holiday_agreed-p) :cond payment_holiday_agreed-p
	  :value (if payment_holiday_agreed 1 0))
	 ((offset_init nil offset_init-p) :cond offset_init-p :value (if offset_init 1 0))
	 ((offset_agreed nil offset_agreed-p) :cond offset_agreed-p :value (if offset_agreed 1 0))
	 ((borrow_back_init nil borrow_back_init-p) :cond borrow_back_init-p :value (if borrow_back_init 1 0))
	 ((borrow_back_agreed nil borrow_back_agreed-p) :cond borrow_back_agreed-p
	  :value (if borrow_back_agreed 1 0))
	 ((speed_completion_init nil speed_completion_init-p) :cond speed_completion_init-p
	  :value (if speed_completion_init 1 0))
	 ((speed_completion_agreed nil speed_completion_agreed-p) :cond speed_completion_agreed-p
	  :value (if speed_completion_agreed 1 0))
	 ((portable_init nil portable_init-p) :cond portable_init-p :value (if portable_init 1 0))
	 ((portable_agreed nil portable_agreed-p) :cond portable_agreed-p :value (if portable_agreed 1 0))
	 ((flexible_insurance_init nil flexible_insurance_init-p) :cond flexible_insurance_init-p
	  :value (if flexible_insurance_init 1 0))
	 ((flexible_insurance_agreed nil flexible_insurance_agreed-p)
	  :cond flexible_insurance_agreed-p :value (if flexible_insurance_agreed 1 0))))
    :patch-defaults
    (:default-args
	(fixing_payment_note paying_upper_limit_note no_tie_in_note avoid_stepped_note low_init_fee_note
	 add_fee_note free_legal_fee_note refund_fee_note no_higher_lending_charge_note cash_back_note
	 over_payment_note under_payment_note payment_holiday_note offset_note borrow_back_note
	 speed_completion_note portable_note flexible_insurance_note
	 ((fixing_payment_init nil fixing_payment_init-p) :cond fixing_payment_init-p
	  :value (if fixing_payment_init 1 0))
	 ((fixing_payment_agreed nil fixing_payment_agreed-p) :cond fixing_payment_agreed-p
	  :value (if fixing_payment_agreed 1 0))
	 ((paying_upper_limit_init nil paying_upper_limit_init-p) :cond paying_upper_limit_init-p
	  :value (if paying_upper_limit_init 1 0))
	 ((paying_upper_limit_agreed nil paying_upper_limit_agreed-p) :cond paying_upper_limit_agreed-p
	  :value (if paying_upper_limit_agreed 1 0))
	 ((no_tie_in_init nil no_tie_in_init-p) :cond no_tie_in_init-p :value (if no_tie_in_init 1 0))
	 ((no_tie_in_agreed nil no_tie_in_agreed-p) :cond no_tie_in_agreed-p :value (if no_tie_in_agreed 1 0))
	 ((avoid_stepped_init nil avoid_stepped_init-p) :cond avoid_stepped_init-p
	  :value (if avoid_stepped_init 1 0))
	 ((avoid_stepped_agreed nil avoid_stepped_agreed-p) :cond avoid_stepped_agreed-p
	  :value (if avoid_stepped_agreed 1 0))
	 ((low_init_fee_init nil low_init_fee_init-p) :cond low_init_fee_init-p :value (if low_init_fee_init 1 0))
	 ((low_init_fee_agreed nil low_init_fee_agreed-p) :cond low_init_fee_agreed-p
	  :value (if low_init_fee_agreed 1 0))
	 ((add_fee_init nil add_fee_init-p) :cond add_fee_init-p :value (if add_fee_init 1 0))
	 ((add_fee_agreed nil add_fee_agreed-p) :cond add_fee_agreed-p :value (if add_fee_agreed 1 0))
	 ((free_legal_fee_init nil free_legal_fee_init-p) :cond free_legal_fee_init-p
	  :value (if free_legal_fee_init 1 0))
	 ((free_legal_fee_agreed nil free_legal_fee_agreed-p) :cond free_legal_fee_agreed-p
	  :value (if free_legal_fee_agreed 1 0))
	 ((refund_fee_init nil refund_fee_init-p) :cond refund_fee_init-p :value (if refund_fee_init 1 0))
	 ((refund_fee_agreed nil refund_fee_agreed-p) :cond refund_fee_agreed-p :value (if refund_fee_agreed 1 0))
	 ((no_higher_lending_charge_init nil no_higher_lending_charge_init-p) :cond no_higher_lending_charge_init-p
	  :value (if no_higher_lending_charge_init 1 0))
	 ((no_higher_lending_charge_agreed nil no_higher_lending_charge_agreed-p)
	  :cond no_higher_lending_charge_agreed-p :value (if no_higher_lending_charge_agreed 1 0))
	 ((cash_back_init nil cash_back_init-p) :cond cash_back_init-p :value (if cash_back_init 1 0))
	 ((cash_back_agreed nil cash_back_agreed-p) :cond cash_back_agreed-p :value (if cash_back_agreed 1 0))
	 ((over_payment_init nil over_payment_init-p) :cond over_payment_init-p :value (if over_payment_init 1 0))
	 ((over_payment_agreed nil over_payment_agreed-p) :cond over_payment_agreed-p
	  :value (if over_payment_agreed 1 0))
	 ((under_payment_init nil under_payment_init-p) :cond under_payment_init-p
	  :value (if under_payment_init 1 0))
	 ((under_payment_agreed nil under_payment_agreed-p) :cond under_payment_agreed-p
	  :value (if under_payment_agreed 1 0))
	 ((payment_holiday_init nil payment_holiday_init-p) :cond payment_holiday_init-p
	  :value (if payment_holiday_init 1 0))
	 ((payment_holiday_agreed nil payment_holiday_agreed-p) :cond payment_holiday_agreed-p
	  :value (if payment_holiday_agreed 1 0))
	 ((offset_init nil offset_init-p) :cond offset_init-p :value (if offset_init 1 0))
	 ((offset_agreed nil offset_agreed-p) :cond offset_agreed-p :value (if offset_agreed 1 0))
	 ((borrow_back_init nil borrow_back_init-p) :cond borrow_back_init-p :value (if borrow_back_init 1 0))
	 ((borrow_back_agreed nil borrow_back_agreed-p) :cond borrow_back_agreed-p
	  :value (if borrow_back_agreed 1 0))
	 ((speed_completion_init nil speed_completion_init-p) :cond speed_completion_init-p
	  :value (if speed_completion_init 1 0))
	 ((speed_completion_agreed nil speed_completion_agreed-p) :cond speed_completion_agreed-p
	  :value (if speed_completion_agreed 1 0))
	 ((portable_init nil portable_init-p) :cond portable_init-p :value (if portable_init 1 0))
	 ((portable_agreed nil portable_agreed-p) :cond portable_agreed-p :value (if portable_agreed 1 0))
	 ((flexible_insurance_init nil flexible_insurance_init-p) :cond flexible_insurance_init-p
	  :value (if flexible_insurance_init 1 0))
	 ((flexible_insurance_agreed nil flexible_insurance_agreed-p)
	  :cond flexible_insurance_agreed-p :value (if flexible_insurance_agreed 1 0))))
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints (entity/client-v2/debt_qualifier/gb/scenario/mortgage_preference
				 "debt_qualifier/gb/scenario" "/entity/client-v2" "mortgage_preference")
    :request-defaults (:list-obj-field scenario_id :inhibit-subitemid t :single-parms-as-body t)
    :get-defaults (:default-args nil)
    :post-defaults
    (:default-args
	(fixing_payment_note paying_upper_limit_note no_tie_in_note avoid_stepped_note low_init_fee_note
	 add_fee_note free_legal_fee_note refund_fee_note no_higher_lending_charge_note cash_back_note
	 over_payment_note under_payment_note payment_holiday_note offset_note borrow_back_note
	 speed_completion_note portable_note flexible_insurance_note
	 ((fixing_payment_init nil fixing_payment_init-p) :cond fixing_payment_init-p
	  :value (if fixing_payment_init 1 0))
	 ((fixing_payment_agreed nil fixing_payment_agreed-p) :cond fixing_payment_agreed-p
	  :value (if fixing_payment_agreed 1 0))
	 ((paying_upper_limit_init nil paying_upper_limit_init-p) :cond paying_upper_limit_init-p
	  :value (if paying_upper_limit_init 1 0))
	 ((paying_upper_limit_agreed nil paying_upper_limit_agreed-p) :cond paying_upper_limit_agreed-p
	  :value (if paying_upper_limit_agreed 1 0))
	 ((no_tie_in_init nil no_tie_in_init-p) :cond no_tie_in_init-p :value (if no_tie_in_init 1 0))
	 ((no_tie_in_agreed nil no_tie_in_agreed-p) :cond no_tie_in_agreed-p :value (if no_tie_in_agreed 1 0))
	 ((avoid_stepped_init nil avoid_stepped_init-p) :cond avoid_stepped_init-p
	  :value (if avoid_stepped_init 1 0))
	 ((avoid_stepped_agreed nil avoid_stepped_agreed-p) :cond avoid_stepped_agreed-p
	  :value (if avoid_stepped_agreed 1 0))
	 ((low_init_fee_init nil low_init_fee_init-p) :cond low_init_fee_init-p :value (if low_init_fee_init 1 0))
	 ((low_init_fee_agreed nil low_init_fee_agreed-p) :cond low_init_fee_agreed-p
	  :value (if low_init_fee_agreed 1 0))
	 ((add_fee_init nil add_fee_init-p) :cond add_fee_init-p :value (if add_fee_init 1 0))
	 ((add_fee_agreed nil add_fee_agreed-p) :cond add_fee_agreed-p :value (if add_fee_agreed 1 0))
	 ((free_legal_fee_init nil free_legal_fee_init-p) :cond free_legal_fee_init-p
	  :value (if free_legal_fee_init 1 0))
	 ((free_legal_fee_agreed nil free_legal_fee_agreed-p) :cond free_legal_fee_agreed-p
	  :value (if free_legal_fee_agreed 1 0))
	 ((refund_fee_init nil refund_fee_init-p) :cond refund_fee_init-p :value (if refund_fee_init 1 0))
	 ((refund_fee_agreed nil refund_fee_agreed-p) :cond refund_fee_agreed-p :value (if refund_fee_agreed 1 0))
	 ((no_higher_lending_charge_init nil no_higher_lending_charge_init-p) :cond no_higher_lending_charge_init-p
	  :value (if no_higher_lending_charge_init 1 0))
	 ((no_higher_lending_charge_agreed nil no_higher_lending_charge_agreed-p)
	  :cond no_higher_lending_charge_agreed-p :value (if no_higher_lending_charge_agreed 1 0))
	 ((cash_back_init nil cash_back_init-p) :cond cash_back_init-p :value (if cash_back_init 1 0))
	 ((cash_back_agreed nil cash_back_agreed-p) :cond cash_back_agreed-p :value (if cash_back_agreed 1 0))
	 ((over_payment_init nil over_payment_init-p) :cond over_payment_init-p :value (if over_payment_init 1 0))
	 ((over_payment_agreed nil over_payment_agreed-p) :cond over_payment_agreed-p
	  :value (if over_payment_agreed 1 0))
	 ((under_payment_init nil under_payment_init-p) :cond under_payment_init-p
	  :value (if under_payment_init 1 0))
	 ((under_payment_agreed nil under_payment_agreed-p) :cond under_payment_agreed-p
	  :value (if under_payment_agreed 1 0))
	 ((payment_holiday_init nil payment_holiday_init-p) :cond payment_holiday_init-p
	  :value (if payment_holiday_init 1 0))
	 ((payment_holiday_agreed nil payment_holiday_agreed-p) :cond payment_holiday_agreed-p
	  :value (if payment_holiday_agreed 1 0))
	 ((offset_init nil offset_init-p) :cond offset_init-p :value (if offset_init 1 0))
	 ((offset_agreed nil offset_agreed-p) :cond offset_agreed-p :value (if offset_agreed 1 0))
	 ((borrow_back_init nil borrow_back_init-p) :cond borrow_back_init-p :value (if borrow_back_init 1 0))
	 ((borrow_back_agreed nil borrow_back_agreed-p) :cond borrow_back_agreed-p
	  :value (if borrow_back_agreed 1 0))
	 ((speed_completion_init nil speed_completion_init-p) :cond speed_completion_init-p
	  :value (if speed_completion_init 1 0))
	 ((speed_completion_agreed nil speed_completion_agreed-p) :cond speed_completion_agreed-p
	  :value (if speed_completion_agreed 1 0))
	 ((portable_init nil portable_init-p) :cond portable_init-p :value (if portable_init 1 0))
	 ((portable_agreed nil portable_agreed-p) :cond portable_agreed-p :value (if portable_agreed 1 0))
	 ((flexible_insurance_init nil flexible_insurance_init-p) :cond flexible_insurance_init-p
	  :value (if flexible_insurance_init 1 0))
	 ((flexible_insurance_agreed nil flexible_insurance_agreed-p)
	  :cond flexible_insurance_agreed-p :value (if flexible_insurance_agreed 1 0))))
    :patch-defaults
    (:default-args
	(fixing_payment_note paying_upper_limit_note no_tie_in_note avoid_stepped_note low_init_fee_note
	 add_fee_note free_legal_fee_note refund_fee_note no_higher_lending_charge_note cash_back_note
	 over_payment_note under_payment_note payment_holiday_note offset_note borrow_back_note
	 speed_completion_note portable_note flexible_insurance_note
	 ((fixing_payment_init nil fixing_payment_init-p) :cond fixing_payment_init-p
	  :value (if fixing_payment_init 1 0))
	 ((fixing_payment_agreed nil fixing_payment_agreed-p) :cond fixing_payment_agreed-p
	  :value (if fixing_payment_agreed 1 0))
	 ((paying_upper_limit_init nil paying_upper_limit_init-p) :cond paying_upper_limit_init-p
	  :value (if paying_upper_limit_init 1 0))
	 ((paying_upper_limit_agreed nil paying_upper_limit_agreed-p) :cond paying_upper_limit_agreed-p
	  :value (if paying_upper_limit_agreed 1 0))
	 ((no_tie_in_init nil no_tie_in_init-p) :cond no_tie_in_init-p :value (if no_tie_in_init 1 0))
	 ((no_tie_in_agreed nil no_tie_in_agreed-p) :cond no_tie_in_agreed-p :value (if no_tie_in_agreed 1 0))
	 ((avoid_stepped_init nil avoid_stepped_init-p) :cond avoid_stepped_init-p
	  :value (if avoid_stepped_init 1 0))
	 ((avoid_stepped_agreed nil avoid_stepped_agreed-p) :cond avoid_stepped_agreed-p
	  :value (if avoid_stepped_agreed 1 0))
	 ((low_init_fee_init nil low_init_fee_init-p) :cond low_init_fee_init-p :value (if low_init_fee_init 1 0))
	 ((low_init_fee_agreed nil low_init_fee_agreed-p) :cond low_init_fee_agreed-p
	  :value (if low_init_fee_agreed 1 0))
	 ((add_fee_init nil add_fee_init-p) :cond add_fee_init-p :value (if add_fee_init 1 0))
	 ((add_fee_agreed nil add_fee_agreed-p) :cond add_fee_agreed-p :value (if add_fee_agreed 1 0))
	 ((free_legal_fee_init nil free_legal_fee_init-p) :cond free_legal_fee_init-p
	  :value (if free_legal_fee_init 1 0))
	 ((free_legal_fee_agreed nil free_legal_fee_agreed-p) :cond free_legal_fee_agreed-p
	  :value (if free_legal_fee_agreed 1 0))
	 ((refund_fee_init nil refund_fee_init-p) :cond refund_fee_init-p :value (if refund_fee_init 1 0))
	 ((refund_fee_agreed nil refund_fee_agreed-p) :cond refund_fee_agreed-p :value (if refund_fee_agreed 1 0))
	 ((no_higher_lending_charge_init nil no_higher_lending_charge_init-p) :cond no_higher_lending_charge_init-p
	  :value (if no_higher_lending_charge_init 1 0))
	 ((no_higher_lending_charge_agreed nil no_higher_lending_charge_agreed-p)
	  :cond no_higher_lending_charge_agreed-p :value (if no_higher_lending_charge_agreed 1 0))
	 ((cash_back_init nil cash_back_init-p) :cond cash_back_init-p :value (if cash_back_init 1 0))
	 ((cash_back_agreed nil cash_back_agreed-p) :cond cash_back_agreed-p :value (if cash_back_agreed 1 0))
	 ((over_payment_init nil over_payment_init-p) :cond over_payment_init-p :value (if over_payment_init 1 0))
	 ((over_payment_agreed nil over_payment_agreed-p) :cond over_payment_agreed-p
	  :value (if over_payment_agreed 1 0))
	 ((under_payment_init nil under_payment_init-p) :cond under_payment_init-p
	  :value (if under_payment_init 1 0))
	 ((under_payment_agreed nil under_payment_agreed-p) :cond under_payment_agreed-p
	  :value (if under_payment_agreed 1 0))
	 ((payment_holiday_init nil payment_holiday_init-p) :cond payment_holiday_init-p
	  :value (if payment_holiday_init 1 0))
	 ((payment_holiday_agreed nil payment_holiday_agreed-p) :cond payment_holiday_agreed-p
	  :value (if payment_holiday_agreed 1 0))
	 ((offset_init nil offset_init-p) :cond offset_init-p :value (if offset_init 1 0))
	 ((offset_agreed nil offset_agreed-p) :cond offset_agreed-p :value (if offset_agreed 1 0))
	 ((borrow_back_init nil borrow_back_init-p) :cond borrow_back_init-p :value (if borrow_back_init 1 0))
	 ((borrow_back_agreed nil borrow_back_agreed-p) :cond borrow_back_agreed-p
	  :value (if borrow_back_agreed 1 0))
	 ((speed_completion_init nil speed_completion_init-p) :cond speed_completion_init-p
	  :value (if speed_completion_init 1 0))
	 ((speed_completion_agreed nil speed_completion_agreed-p) :cond speed_completion_agreed-p
	  :value (if speed_completion_agreed 1 0))
	 ((portable_init nil portable_init-p) :cond portable_init-p :value (if portable_init 1 0))
	 ((portable_agreed nil portable_agreed-p) :cond portable_agreed-p :value (if portable_agreed 1 0))
	 ((flexible_insurance_init nil flexible_insurance_init-p) :cond flexible_insurance_init-p
	  :value (if flexible_insurance_init 1 0))
	 ((flexible_insurance_agreed nil flexible_insurance_agreed-p)
	  :cond flexible_insurance_agreed-p :value (if flexible_insurance_agreed 1 0))))
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints (entity/client-v3/debt_qualifier/gb/scenario/mortgage_preference
				 "debt_qualifier/gb/scenario" "/entity/client-v3" "mortgage_preference")
    :request-defaults (:list-obj-field scenario_id :inhibit-subitemid t :single-parms-as-body t)
    :get-defaults (:default-args nil)
    :post-defaults
    (:default-args
	(fixing_payment_note paying_upper_limit_note no_tie_in_note avoid_stepped_note low_init_fee_note
	 add_fee_note free_legal_fee_note refund_fee_note no_higher_lending_charge_note cash_back_note
	 over_payment_note under_payment_note payment_holiday_note offset_note borrow_back_note
	 speed_completion_note portable_note flexible_insurance_note
	 ((fixing_payment_init nil fixing_payment_init-p) :cond fixing_payment_init-p
	  :value (if fixing_payment_init 1 0))
	 ((fixing_payment_agreed nil fixing_payment_agreed-p) :cond fixing_payment_agreed-p
	  :value (if fixing_payment_agreed 1 0))
	 ((paying_upper_limit_init nil paying_upper_limit_init-p) :cond paying_upper_limit_init-p
	  :value (if paying_upper_limit_init 1 0))
	 ((paying_upper_limit_agreed nil paying_upper_limit_agreed-p) :cond paying_upper_limit_agreed-p
	  :value (if paying_upper_limit_agreed 1 0))
	 ((no_tie_in_init nil no_tie_in_init-p) :cond no_tie_in_init-p :value (if no_tie_in_init 1 0))
	 ((no_tie_in_agreed nil no_tie_in_agreed-p) :cond no_tie_in_agreed-p :value (if no_tie_in_agreed 1 0))
	 ((avoid_stepped_init nil avoid_stepped_init-p) :cond avoid_stepped_init-p
	  :value (if avoid_stepped_init 1 0))
	 ((avoid_stepped_agreed nil avoid_stepped_agreed-p) :cond avoid_stepped_agreed-p
	  :value (if avoid_stepped_agreed 1 0))
	 ((low_init_fee_init nil low_init_fee_init-p) :cond low_init_fee_init-p :value (if low_init_fee_init 1 0))
	 ((low_init_fee_agreed nil low_init_fee_agreed-p) :cond low_init_fee_agreed-p
	  :value (if low_init_fee_agreed 1 0))
	 ((add_fee_init nil add_fee_init-p) :cond add_fee_init-p :value (if add_fee_init 1 0))
	 ((add_fee_agreed nil add_fee_agreed-p) :cond add_fee_agreed-p :value (if add_fee_agreed 1 0))
	 ((free_legal_fee_init nil free_legal_fee_init-p) :cond free_legal_fee_init-p
	  :value (if free_legal_fee_init 1 0))
	 ((free_legal_fee_agreed nil free_legal_fee_agreed-p) :cond free_legal_fee_agreed-p
	  :value (if free_legal_fee_agreed 1 0))
	 ((refund_fee_init nil refund_fee_init-p) :cond refund_fee_init-p :value (if refund_fee_init 1 0))
	 ((refund_fee_agreed nil refund_fee_agreed-p) :cond refund_fee_agreed-p :value (if refund_fee_agreed 1 0))
	 ((no_higher_lending_charge_init nil no_higher_lending_charge_init-p) :cond no_higher_lending_charge_init-p
	  :value (if no_higher_lending_charge_init 1 0))
	 ((no_higher_lending_charge_agreed nil no_higher_lending_charge_agreed-p)
	  :cond no_higher_lending_charge_agreed-p :value (if no_higher_lending_charge_agreed 1 0))
	 ((cash_back_init nil cash_back_init-p) :cond cash_back_init-p :value (if cash_back_init 1 0))
	 ((cash_back_agreed nil cash_back_agreed-p) :cond cash_back_agreed-p :value (if cash_back_agreed 1 0))
	 ((over_payment_init nil over_payment_init-p) :cond over_payment_init-p :value (if over_payment_init 1 0))
	 ((over_payment_agreed nil over_payment_agreed-p) :cond over_payment_agreed-p
	  :value (if over_payment_agreed 1 0))
	 ((under_payment_init nil under_payment_init-p) :cond under_payment_init-p
	  :value (if under_payment_init 1 0))
	 ((under_payment_agreed nil under_payment_agreed-p) :cond under_payment_agreed-p
	  :value (if under_payment_agreed 1 0))
	 ((payment_holiday_init nil payment_holiday_init-p) :cond payment_holiday_init-p
	  :value (if payment_holiday_init 1 0))
	 ((payment_holiday_agreed nil payment_holiday_agreed-p) :cond payment_holiday_agreed-p
	  :value (if payment_holiday_agreed 1 0))
	 ((offset_init nil offset_init-p) :cond offset_init-p :value (if offset_init 1 0))
	 ((offset_agreed nil offset_agreed-p) :cond offset_agreed-p :value (if offset_agreed 1 0))
	 ((borrow_back_init nil borrow_back_init-p) :cond borrow_back_init-p :value (if borrow_back_init 1 0))
	 ((borrow_back_agreed nil borrow_back_agreed-p) :cond borrow_back_agreed-p
	  :value (if borrow_back_agreed 1 0))
	 ((speed_completion_init nil speed_completion_init-p) :cond speed_completion_init-p
	  :value (if speed_completion_init 1 0))
	 ((speed_completion_agreed nil speed_completion_agreed-p) :cond speed_completion_agreed-p
	  :value (if speed_completion_agreed 1 0))
	 ((portable_init nil portable_init-p) :cond portable_init-p :value (if portable_init 1 0))
	 ((portable_agreed nil portable_agreed-p) :cond portable_agreed-p :value (if portable_agreed 1 0))
	 ((flexible_insurance_init nil flexible_insurance_init-p) :cond flexible_insurance_init-p
	  :value (if flexible_insurance_init 1 0))
	 ((flexible_insurance_agreed nil flexible_insurance_agreed-p)
	  :cond flexible_insurance_agreed-p :value (if flexible_insurance_agreed 1 0))))
    :patch-defaults
    (:default-args
	(fixing_payment_note paying_upper_limit_note no_tie_in_note avoid_stepped_note low_init_fee_note
	 add_fee_note free_legal_fee_note refund_fee_note no_higher_lending_charge_note cash_back_note
	 over_payment_note under_payment_note payment_holiday_note offset_note borrow_back_note
	 speed_completion_note portable_note flexible_insurance_note
	 ((fixing_payment_init nil fixing_payment_init-p) :cond fixing_payment_init-p
	  :value (if fixing_payment_init 1 0))
	 ((fixing_payment_agreed nil fixing_payment_agreed-p) :cond fixing_payment_agreed-p
	  :value (if fixing_payment_agreed 1 0))
	 ((paying_upper_limit_init nil paying_upper_limit_init-p) :cond paying_upper_limit_init-p
	  :value (if paying_upper_limit_init 1 0))
	 ((paying_upper_limit_agreed nil paying_upper_limit_agreed-p) :cond paying_upper_limit_agreed-p
	  :value (if paying_upper_limit_agreed 1 0))
	 ((no_tie_in_init nil no_tie_in_init-p) :cond no_tie_in_init-p :value (if no_tie_in_init 1 0))
	 ((no_tie_in_agreed nil no_tie_in_agreed-p) :cond no_tie_in_agreed-p :value (if no_tie_in_agreed 1 0))
	 ((avoid_stepped_init nil avoid_stepped_init-p) :cond avoid_stepped_init-p
	  :value (if avoid_stepped_init 1 0))
	 ((avoid_stepped_agreed nil avoid_stepped_agreed-p) :cond avoid_stepped_agreed-p
	  :value (if avoid_stepped_agreed 1 0))
	 ((low_init_fee_init nil low_init_fee_init-p) :cond low_init_fee_init-p :value (if low_init_fee_init 1 0))
	 ((low_init_fee_agreed nil low_init_fee_agreed-p) :cond low_init_fee_agreed-p
	  :value (if low_init_fee_agreed 1 0))
	 ((add_fee_init nil add_fee_init-p) :cond add_fee_init-p :value (if add_fee_init 1 0))
	 ((add_fee_agreed nil add_fee_agreed-p) :cond add_fee_agreed-p :value (if add_fee_agreed 1 0))
	 ((free_legal_fee_init nil free_legal_fee_init-p) :cond free_legal_fee_init-p
	  :value (if free_legal_fee_init 1 0))
	 ((free_legal_fee_agreed nil free_legal_fee_agreed-p) :cond free_legal_fee_agreed-p
	  :value (if free_legal_fee_agreed 1 0))
	 ((refund_fee_init nil refund_fee_init-p) :cond refund_fee_init-p :value (if refund_fee_init 1 0))
	 ((refund_fee_agreed nil refund_fee_agreed-p) :cond refund_fee_agreed-p :value (if refund_fee_agreed 1 0))
	 ((no_higher_lending_charge_init nil no_higher_lending_charge_init-p) :cond no_higher_lending_charge_init-p
	  :value (if no_higher_lending_charge_init 1 0))
	 ((no_higher_lending_charge_agreed nil no_higher_lending_charge_agreed-p)
	  :cond no_higher_lending_charge_agreed-p :value (if no_higher_lending_charge_agreed 1 0))
	 ((cash_back_init nil cash_back_init-p) :cond cash_back_init-p :value (if cash_back_init 1 0))
	 ((cash_back_agreed nil cash_back_agreed-p) :cond cash_back_agreed-p :value (if cash_back_agreed 1 0))
	 ((over_payment_init nil over_payment_init-p) :cond over_payment_init-p :value (if over_payment_init 1 0))
	 ((over_payment_agreed nil over_payment_agreed-p) :cond over_payment_agreed-p
	  :value (if over_payment_agreed 1 0))
	 ((under_payment_init nil under_payment_init-p) :cond under_payment_init-p
	  :value (if under_payment_init 1 0))
	 ((under_payment_agreed nil under_payment_agreed-p) :cond under_payment_agreed-p
	  :value (if under_payment_agreed 1 0))
	 ((payment_holiday_init nil payment_holiday_init-p) :cond payment_holiday_init-p
	  :value (if payment_holiday_init 1 0))
	 ((payment_holiday_agreed nil payment_holiday_agreed-p) :cond payment_holiday_agreed-p
	  :value (if payment_holiday_agreed 1 0))
	 ((offset_init nil offset_init-p) :cond offset_init-p :value (if offset_init 1 0))
	 ((offset_agreed nil offset_agreed-p) :cond offset_agreed-p :value (if offset_agreed 1 0))
	 ((borrow_back_init nil borrow_back_init-p) :cond borrow_back_init-p :value (if borrow_back_init 1 0))
	 ((borrow_back_agreed nil borrow_back_agreed-p) :cond borrow_back_agreed-p
	  :value (if borrow_back_agreed 1 0))
	 ((speed_completion_init nil speed_completion_init-p) :cond speed_completion_init-p
	  :value (if speed_completion_init 1 0))
	 ((speed_completion_agreed nil speed_completion_agreed-p) :cond speed_completion_agreed-p
	  :value (if speed_completion_agreed 1 0))
	 ((portable_init nil portable_init-p) :cond portable_init-p :value (if portable_init 1 0))
	 ((portable_agreed nil portable_agreed-p) :cond portable_agreed-p :value (if portable_agreed 1 0))
	 ((flexible_insurance_init nil flexible_insurance_init-p) :cond flexible_insurance_init-p
	  :value (if flexible_insurance_init 1 0))
	 ((flexible_insurance_agreed nil flexible_insurance_agreed-p)
	  :cond flexible_insurance_agreed-p :value (if flexible_insurance_agreed 1 0))))
    :put-defaults (:inhibit t))
