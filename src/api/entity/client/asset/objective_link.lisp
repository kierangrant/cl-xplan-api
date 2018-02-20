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

File: src/api/entity/client/asset/objective_link.lisp
Description: /entity/client/asset/objective_link API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/asset/objective_link "asset" "/entity/client" "objective_link")
    :request-defaults (:inhibit t :sublist-obj-field objective_obj_index)
    :get-defaults (:default-args nil :inhibit nil)
    :post-defaults (:default-args (objective_obj_index) :inhibit nil)
    :delete-defaults (:inhibit nil))

(define-dynamiclike-entrypoints (entity/client-v2/asset/objective_link "asset" "/entity/client-v2" "objective_link")
    :request-defaults (:inhibit t :sublist-obj-field objective_obj_index)
    :get-defaults (:default-args nil :inhibit nil)
    :post-defaults (:default-args (objective_obj_index) :inhibit nil)
    :delete-defaults (:inhibit nil))

(define-dynamiclike-entrypoints (entity/client-v3/asset/objective_link "asset" "/entity/client-v3" "objective_link")
    :request-defaults (:inhibit t :sublist-obj-field objective_obj_index)
    :get-defaults (:default-args nil :inhibit nil)
    :post-defaults (:default-args (objective_obj_index) :inhibit nil)
    :delete-defaults (:inhibit nil))
