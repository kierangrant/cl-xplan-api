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

File: src/api/entity/client/debt_qualifier/gb/scenario/product_attachment.lisp
Description: /entity/client/debt_qualifier/gb/scenario/product_attachment API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/debt_qualifier/gb/scenario/product_attachment
				 "debt_qualifier/gb/scenario" "/entity/client" "product_attachment")
    :request-defaults (:inhibit t :list-obj-field scenario_id :sublist-obj-field attachment_id)
    :get-defaults (:inhibit nil :default-args ((product_id :cond (and product_id (not attachment_id)))))
    :delete-defaults (:inhibit nil))

(define-dynamiclike-entrypoints (entity/client-v2/debt_qualifier/gb/scenario/product_attachment
				 "debt_qualifier/gb/scenario" "/entity/client-v2" "product_attachment")
    :request-defaults (:inhibit t :list-obj-field scenario_id :sublist-obj-field attachment_id)
    :get-defaults (:inhibit nil :default-args ((product_id :cond (and product_id (not attachment_id)))))
    :delete-defaults (:inhibit nil))

(define-dynamiclike-entrypoints (entity/client-v3/debt_qualifier/gb/scenario/product_attachment
				 "debt_qualifier/gb/scenario" "/entity/client-v3" "product_attachment")
    :request-defaults (:inhibit t :list-obj-field scenario_id :sublist-obj-field attachment_id)
    :get-defaults (:inhibit nil :default-args ((product_id :cond (and product_id (not attachment_id)))))
    :delete-defaults (:inhibit nil))
