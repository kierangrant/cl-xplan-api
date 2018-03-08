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

File: src/api/entity/client/cashflow/employment.lisp
Description: /entity/client/cashflow/employment API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/cashflow/employment "cashflow" "/entity/client" "employment")
    :request-defaults (:inhibit t :list-obj-field cashflow_index :sublist-obj-field employment_index)
    :get-defaults (:inhibit nil :default-args nil :inhibit-subitemid T)
    :delete-defaults (:inhibit nil))

(define-entrypoint entity/client/cashflow/employment :post
  (entity_id cashflow_index employment_index) ()
  :resource (format nil "/entity/client/~A/cashflow/~A/employment/~A" entity_id cashflow_index employment_index))

(define-dynamiclike-entrypoints (entity/client-v2/cashflow/employment "cashflow" "/entity/client-v2" "employment")
    :request-defaults (:inhibit t :list-obj-field cashflow_index :sublist-obj-field employment_index)
    :get-defaults (:inhibit nil :default-args nil :inhibit-subitemid T)
    :delete-defaults (:inhibit nil))

(define-entrypoint entity/client-v2/cashflow/employment :post
  (entity_id cashflow_index employment_index) ()
  :resource (format nil "/entity/client-v2/~A/cashflow/~A/employment/~A"
		    entity_id cashflow_index employment_index))

(define-dynamiclike-entrypoints (entity/client-v3/cashflow/employment "cashflow" "/entity/client-v3" "employment")
    :request-defaults (:inhibit t :list-obj-field cashflow_index :sublist-obj-field employment_index)
    :get-defaults (:inhibit nil :default-args nil :inhibit-subitemid T)
    :delete-defaults (:inhibit nil))

(define-entrypoint entity/client-v3/cashflow/employment :post
  (entity_id cashflow_index employment_index) ()
  :resource (format nil "/entity/client-v3/~A/cashflow/~A/employment/~A"
		    entity_id cashflow_index employment_index))
