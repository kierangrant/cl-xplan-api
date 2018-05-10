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

File: src/api/entity/client/debt_qualifier/gb/scenario/rate_type.lisp
Description: /entity/client/debt_qualifier/gb/scenario/rate_type API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/debt_qualifier/gb/scenario/rate_type
				 "debt_qualifier/gb/scenario" "/entity/client" "rate_type")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t :single-parms-as-body t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults
    (:inhibit nil :default-args (((all_type nil all_type-p) :cond all_type-p :value (if all_type 1 0))
				 ((fixed nil fixed-p) :cond fixed-p :value (if fixed 1 0))
				 ((variable nil variable-p) :cond variable-p :value (if variable 1 0))
				 ((discounted nil discounted-p) :cond discounted-p :value (if discounted 1 0))
				 ((tracker nil tracker-p) :cond tracker-p :value (if tracker 1 0))
				 ((capped nil capped-p) :cond capped-p :value (if capped 1 0))
				 ((libor nil libor-p) :cond libor-p :value (if libor 1 0)))))

(define-dynamiclike-entrypoints (entity/client-v2/debt_qualifier/gb/scenario/rate_type
				 "debt_qualifier/gb/scenario" "/entity/client-v2" "rate_type")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t :single-parms-as-body t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults
    (:inhibit nil :default-args (((all_type nil all_type-p) :cond all_type-p :value (if all_type 1 0))
				 ((fixed nil fixed-p) :cond fixed-p :value (if fixed 1 0))
				 ((variable nil variable-p) :cond variable-p :value (if variable 1 0))
				 ((discounted nil discounted-p) :cond discounted-p :value (if discounted 1 0))
				 ((tracker nil tracker-p) :cond tracker-p :value (if tracker 1 0))
				 ((capped nil capped-p) :cond capped-p :value (if capped 1 0))
				 ((libor nil libor-p) :cond libor-p :value (if libor 1 0)))))

(define-dynamiclike-entrypoints (entity/client-v3/debt_qualifier/gb/scenario/rate_type
				 "debt_qualifier/gb/scenario" "/entity/client-v3" "rate_type")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t :single-parms-as-body t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults
    (:inhibit nil :default-args (((all_type nil all_type-p) :cond all_type-p :value (if all_type 1 0))
				 ((fixed nil fixed-p) :cond fixed-p :value (if fixed 1 0))
				 ((variable nil variable-p) :cond variable-p :value (if variable 1 0))
				 ((discounted nil discounted-p) :cond discounted-p :value (if discounted 1 0))
				 ((tracker nil tracker-p) :cond tracker-p :value (if tracker 1 0))
				 ((capped nil capped-p) :cond capped-p :value (if capped 1 0))
				 ((libor nil libor-p) :cond libor-p :value (if libor 1 0)))))
