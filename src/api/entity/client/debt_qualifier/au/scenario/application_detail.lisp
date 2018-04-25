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

File: src/api/entity/client/debt_qualifier/au/scenario/application_detail.lisp
Description: /entity/client/debt_qualifier/au/scenario/application_detail API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/debt_qualifier/au/scenario/application_detail
				 "debt_qualifier/au/scenario" "/entity/client" "application_detail")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults (:inhibit nil :default-args (application_type finance_date settlement_date attached forthcoming other comments ((generate_form nil generate_form-p) :cond generate_form-p :value (if generate_form 1 0)) purpose characteristics servicing)))

(define-dynamiclike-entrypoints (entity/client-v2/debt_qualifier/au/scenario/application_detail
				 "debt_qualifier/au/scenario" "/entity/client-v2" "application_detail")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults (:inhibit nil :default-args (application_type finance_date settlement_date attached forthcoming other comments ((generate_form nil generate_form-p) :cond generate_form-p :value (if generate_form 1 0)) purpose characteristics servicing)))

(define-dynamiclike-entrypoints (entity/client-v3/debt_qualifier/au/scenario/application_detail
				 "debt_qualifier/au/scenario" "/entity/client-v3" "application_detail")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults (:inhibit nil :default-args (application_type finance_date settlement_date attached forthcoming other comments ((generate_form nil generate_form-p) :cond generate_form-p :value (if generate_form 1 0)) purpose characteristics servicing)))
