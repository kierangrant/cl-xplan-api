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

File: src/api/entity/client/debt_qualifier/gb/scenario/scenario_attachment.lisp
Description: /entity/client/debt_qualifier/gb/scenario/scenario_attachment API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/debt_qualifier/gb/scenario/scenario_attachment
				 "debt_qualifier/gb/scenario" "/entity/client" "scenario_attachment")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field attachment_id :single-parms-as-body t)
    :get-defaults (:default-args  nil)
    :post-defaults (:default-args (file_name description product_id binary_data mimetype))
    :patch-defaults (:inhibit t) :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints (entity/client-v2/debt_qualifier/gb/scenario/scenario_attachment
				 "debt_qualifier/gb/scenario" "/entity/client-v2" "scenario_attachment")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field attachment_id :single-parms-as-body t)
    :get-defaults (:default-args  nil)
    :post-defaults (:default-args (file_name description product_id binary_data mimetype))
    :patch-defaults (:inhibit t) :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints (entity/client-v3/debt_qualifier/gb/scenario/scenario_attachment
				 "debt_qualifier/gb/scenario" "/entity/client-v3" "scenario_attachment")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field attachment_id :single-parms-as-body t)
    :get-defaults (:default-args  nil)
    :post-defaults (:default-args (file_name description product_id binary_data mimetype))
    :patch-defaults (:inhibit t) :put-defaults (:inhibit t))
