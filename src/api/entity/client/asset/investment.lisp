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

File: src/api/entity/client/asset/investment.lisp
Description: /entity/client/asset/investment API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/asset/investment "asset" "/entity/client" "investment")
    :request-defaults (:list-obj-field fund_id :sublist-obj-field option_id)
    :get-defaults (:default-args (((performance nil performance-p) :cond performance-p :value (if performance 1 0))))
    :post-defaults (:default-args
		       (name option_spin performance_fee buy_cost sell_cost allocations research_id security_code amount))
    :patch-defaults (:default-args (name option_spin performance_fee buy_cost sell_cost allocations amount))
    :put-defaults (:inhibit T))

(define-dynamiclike-entrypoints (entity/client-v2/asset/investment "asset" "/entity/client-v2" "investment")
    :request-defaults (:list-obj-field fund_id :sublist-obj-field option_id)
    :get-defaults (:default-args (((performance nil performance-p) :cond performance-p :value (if performance 1 0))))
    :post-defaults (:default-args
		       (name option_spin performance_fee buy_cost sell_cost allocations research_id security_code amount))
    :patch-defaults (:default-args (name option_spin performance_fee buy_cost sell_cost allocations amount))
    :put-defaults (:inhibit T))

(define-dynamiclike-entrypoints (entity/client-v3/asset/investment "asset" "/entity/client-v3" "investment")
    :request-defaults (:list-obj-field fund_id :sublist-obj-field option_id)
    :get-defaults (:default-args (((performance nil performance-p) :cond performance-p :value (if performance 1 0))))
    :post-defaults (:default-args
		       (name option_spin performance_fee buy_cost sell_cost allocations research_id security_code amount))
    :patch-defaults (:default-args (name option_spin performance_fee buy_cost sell_cost allocations amount))
    :put-defaults (:inhibit T))
