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

File: src/api/entity/client/debt_qualifier/au/scenario/income.lisp
Description: /entity/client/debt_qualifier/au/scenario/income API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints
    (entity/client/debt_qualifier/au/scenario/income "debt_qualifier/au/scenario" "/entity/client" "income")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field income_id)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (income_type income_applicant_id income_amount income_year income_description))
    :put-defaults (:inhibit t)
    :patch-defaults (:default-args (income_type income_applicant_id income_amount income_year income_description)))

(define-dynamiclike-entrypoints
    (entity/client-v2/debt_qualifier/au/scenario/income "debt_qualifier/au/scenario" "/entity/client-v2" "income")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field income_id)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (income_type income_applicant_id income_amount income_year income_description))
    :put-defaults (:inhibit t)
    :patch-defaults (:default-args (income_type income_applicant_id income_amount income_year income_description)))

(define-dynamiclike-entrypoints
    (entity/client-v3/debt_qualifier/au/scenario/income "debt_qualifier/au/scenario" "/entity/client-v3" "income")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field income_id)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (income_type income_applicant_id income_amount income_year income_description))
    :put-defaults (:inhibit t)
    :patch-defaults (:default-args (income_type income_applicant_id income_amount income_year income_description)))
