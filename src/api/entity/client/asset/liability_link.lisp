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

File: src/api/entity/client/asset/liability_link.lisp
Description: /entity/client/asset/liability_link API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/asset/liability_link "asset" "/entity/client" "liability_link")
    :request-defaults (:sublist-obj-field liability_id)
    :post-defaults (:default-args (liability_id link_type))
    :patch-defaults (:default-args (link_type))
    :put-defaults (:inhibit T))

(define-dynamiclike-entrypoints (entity/client-v2/asset/liability_link "asset" "/entity/client-v2" "liability_link")
    :request-defaults (:sublist-obj-field liability_id)
    :post-defaults (:default-args (liability_id link_type))
    :patch-defaults (:default-args (link_type))
    :put-defaults (:inhibit T))

(define-dynamiclike-entrypoints (entity/client-v3/asset/liability_link "asset" "/entity/client-v3" "liability_link")
    :request-defaults (:sublist-obj-field liability_id)
    :post-defaults (:default-args (liability_id link_type))
    :patch-defaults (:default-args (link_type))
    :put-defaults (:inhibit T))
