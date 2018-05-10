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

File: src/api/entity/client/debt_qualifier/gb/scenario/credit_history.lisp
Description: /entity/client/debt_qualifier/gb/scenario/credit_history API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/debt_qualifier/gb/scenario/credit_history
				 "debt_qualifier/gb/scenario" "/entity/client" "credit_history")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field question_id :inhibit t)
    :get-defaults (:default-args () :inhibit nil)
    :patch-defaults (:default-args (((answer nil answer-p) :cond answer-p :value (if answer 1 0))) :inhibit nil))

(define-dynamiclike-entrypoints (entity/client-v2/debt_qualifier/gb/scenario/credit_history
				 "debt_qualifier/gb/scenario" "/entity/client-v2" "credit_history")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field question_id :inhibit t)
    :get-defaults (:default-args () :inhibit nil)
    :patch-defaults (:default-args (((answer nil answer-p) :cond answer-p :value (if answer 1 0))) :inhibit nil))

(define-dynamiclike-entrypoints (entity/client-v3/debt_qualifier/gb/scenario/credit_history
				 "debt_qualifier/gb/scenario" "/entity/client-v3" "credit_history")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field question_id :inhibit t)
    :get-defaults (:default-args () :inhibit nil)
    :patch-defaults (:default-args (((answer nil answer-p) :cond answer-p :value (if answer 1 0))) :inhibit nil))
