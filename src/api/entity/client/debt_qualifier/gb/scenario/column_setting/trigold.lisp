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

File: src/api/entity/client/debt_qualifier/gb/scenario/column_setting/trigold.lisp
Description: /entity/client/debt_qualifier/gb/scenario/column_setting/trigold API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/debt_qualifier/gb/scenario/column_setting/trigold
				 "debt_qualifier/gb/scenario" "/entity/client" "column_setting/trigold")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults (:inhibit nil :default-args (column_visible column_order)))

(define-dynamiclike-entrypoints (entity/client-v2/debt_qualifier/gb/scenario/column_setting/trigold
				 "debt_qualifier/gb/scenario" "/entity/client-v2" "column_setting/trigold")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults (:inhibit nil :default-args (column_visible column_order)))

(define-dynamiclike-entrypoints (entity/client-v3/debt_qualifier/gb/scenario/column_setting/trigold
				 "debt_qualifier/gb/scenario" "/entity/client-v3" "column_setting/trigold")
    :request-defaults (:inhibit t :list-obj-field scenario_id :inhibit-subitemid t)
    :get-defaults (:inhibit nil :default-args nil)
    :patch-defaults (:inhibit nil :default-args (column_visible column_order)))
