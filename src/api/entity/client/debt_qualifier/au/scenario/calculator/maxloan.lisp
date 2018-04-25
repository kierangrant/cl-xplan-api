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

File: src/api/entity/client/debt_qualifier/au/scenario/calculator/maxloan.lisp
Description: /entity/client/debt_qualifier/au/scenario/calculator/maxloan API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/debt_qualifier/au/scenario/calculator/maxloan "debt_qualifier/au/scenario" "/entity/client" "calculator/maxloan")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t)
    :get-defaults (:inhibit nil :default-args (lender_id))
    :patch-defaults (:inhibit nil :default-args (lender_id assessment_term assessment_rate original_assessment_term original_assessment_rate)))

(define-dynamiclike-entrypoints (entity/client-v2/debt_qualifier/au/scenario/calculator/maxloan "debt_qualifier/au/scenario" "/entity/client-v2" "calculator/maxloan")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t)
    :get-defaults (:inhibit nil :default-args (lender_id))
    :patch-defaults (:inhibit nil :default-args (lender_id assessment_term assessment_rate original_assessment_term original_assessment_rate)))

(define-dynamiclike-entrypoints (entity/client-v3/debt_qualifier/au/scenario/calculator/maxloan "debt_qualifier/au/scenario" "/entity/client-v3" "calculator/maxloan")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t)
    :get-defaults (:inhibit nil :default-args (lender_id))
    :patch-defaults (:inhibit nil :default-args (lender_id assessment_term assessment_rate original_assessment_term original_assessment_rate)))
