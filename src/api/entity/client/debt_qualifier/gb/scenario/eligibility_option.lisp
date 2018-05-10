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

File: src/api/entity/client/debt_qualifier/gb/scenario/eligibility_option.lisp
Description: /entity/client/debt_qualifier/gb/scenario/eligibility_option API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/debt_qualifier/gb/scenario/eligibility_option
				 "debt_qualifier/gb/scenario" "/entity/client" "eligibility_option")
    :request-defaults (:list-obj-field scenario_id :inhibit-subitemid t :inhibit t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults (:inhibit nil :default-args (((eligibility_term nil eligibility_term-p)
						  :cond eligibility_term-p :value (if eligibility_term 1 0))
						 ((eligibility_additional_info nil eligibility_additional_info-p)
						  :cond eligibility_additional_info-p
						  :value (if eligibility_additional_info 1 0))
						 ((debug_mode nil debug_mode-p) :cond debug_mode-p
						  :value (if debug_mode 1 0)))))

(define-dynamiclike-entrypoints (entity/client-v2/debt_qualifier/gb/scenario/eligibility_option
				 "debt_qualifier/gb/scenario" "/entity/client-v2" "eligibility_option")
    :request-defaults (:list-obj-field scenario_id :inhibit-subitemid t :inhibit t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults (:inhibit nil :default-args (((eligibility_term nil eligibility_term-p)
						  :cond eligibility_term-p :value (if eligibility_term 1 0))
						 ((eligibility_additional_info nil eligibility_additional_info-p)
						  :cond eligibility_additional_info-p
						  :value (if eligibility_additional_info 1 0))
						 ((debug_mode nil debug_mode-p) :cond debug_mode-p
						  :value (if debug_mode 1 0)))))

(define-dynamiclike-entrypoints (entity/client-v3/debt_qualifier/gb/scenario/eligibility_option
				 "debt_qualifier/gb/scenario" "/entity/client-v3" "eligibility_option")
    :request-defaults (:list-obj-field scenario_id :inhibit-subitemid t :inhibit t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults (:inhibit nil :default-args (((eligibility_term nil eligibility_term-p)
						  :cond eligibility_term-p :value (if eligibility_term 1 0))
						 ((eligibility_additional_info nil eligibility_additional_info-p)
						  :cond eligibility_additional_info-p
						  :value (if eligibility_additional_info 1 0))
						 ((debug_mode nil debug_mode-p) :cond debug_mode-p
						  :value (if debug_mode 1 0)))))
