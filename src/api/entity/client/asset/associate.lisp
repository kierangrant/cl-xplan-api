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

File: src/api/entity/client/asset/associate.lisp
Description: /entity/client/asset/associate API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/asset/associate "asset" "/entity/client" "associate")
    :request-defaults (:sublist-obj-field associate_id)
    :patch-defaults (:inhibit t)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (associate_model_id associate_type))
    :put-defaults (:default-args (associate_model_ids associate_type))
    :delete-defaults (:default-args (((underlying_model nil underlying_model-p) :cond underlying_model-p :value (if underlying_model 1 0)))))

(define-dynamiclike-entrypoints (entity/client-v2/asset/associate "asset" "/entity/client-v2" "associate")
    :request-defaults (:sublist-obj-field associate_id)
    :patch-defaults (:inhibit t)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (associate_model_id associate_type))
    :put-defaults (:default-args (associate_model_ids associate_type))
    :delete-defaults (:default-args (((underlying_model nil underlying_model-p) :cond underlying_model-p :value (if underlying_model 1 0)))))

(define-dynamiclike-entrypoints (entity/client-v3/asset/associate "asset" "/entity/client-v3" "associate")
    :request-defaults (:sublist-obj-field associate_id)
    :patch-defaults (:inhibit t)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (associate_model_id associate_type))
    :put-defaults (:default-args (associate_model_ids associate_type))
    :delete-defaults (:default-args (((underlying_model nil underlying_model-p) :cond underlying_model-p :value (if underlying_model 1 0)))))
