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

File: src/api/entity/client/debt_qualifier/au/scenario/summary_note.lisp
Description: /entity/client/debt_qualifier/au/scenario/summary_note API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints
    (entity/client/debt_qualifier/au/scenario/summary_note "debt_qualifier/au/scenario" "/entity/client"
							   "summary_note")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults (:inhibit nil :default-args (why_lender_selected loan_structure)))

(define-dynamiclike-entrypoints
    (entity/client-v2/debt_qualifier/au/scenario/summary_note "debt_qualifier/au/scenario" "/entity/client-v2"
							      "summary_note")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults (:inhibit nil :default-args (why_lender_selected loan_structure)))

(define-dynamiclike-entrypoints
    (entity/client-v3/debt_qualifier/au/scenario/summary_note "debt_qualifier/au/scenario" "/entity/client-v3"
							      "summary_note")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults (:inhibit nil :default-args (why_lender_selected loan_structure)))
