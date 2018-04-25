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

File: src/api/entity/client/debt_qualifier/au/scenario/expense.lisp
Description: /entity/client/debt_qualifier/au/scenario/expense API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints
    (entity/client/debt_qualifier/au/scenario/expense "debt_qualifier/au/scenario" "/entity/client" "expense")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field expense_id)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (expense_type expense_applicant_id expense_amount expense_year
						expense_description))
    :patch-defaults (:default-args (expense_type expense_applicant_id expense_amount expense_year
						 expense_description))
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints
    (entity/client-v3/debt_qualifier/au/scenario/expense "debt_qualifier/au/scenario" "/entity/client-v3"
							 "expense")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field expense_id)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (expense_type expense_applicant_id expense_amount expense_year
						expense_description))
    :patch-defaults (:default-args (expense_type expense_applicant_id expense_amount expense_year
						 expense_description))
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints
    (entity/client-v3/debt_qualifier/au/scenario/expense "debt_qualifier/au/scenario" "/entity/client-v3"
							 "expense")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field expense_id)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (expense_type expense_applicant_id expense_amount expense_year
						expense_description))
    :patch-defaults (:default-args (expense_type expense_applicant_id expense_amount expense_year
						 expense_description))
    :put-defaults (:inhibit t))
