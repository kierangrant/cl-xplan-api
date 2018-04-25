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

File: src/api/entity/client/debt_qualifier/au/scenario.lisp
Description: /entity/client/debt_qualifier/au/scenario API functions
|#

(in-package :cl-xplan-api/api)

;; client

(define-dynamiclike-entrypoints (entity/client/debt_qualifier/au/scenario "debt_qualifier/au/scenario" "/entity/client")
    :request-defaults (:list-obj-field scenario_id)
    :put-defaults (:inhibit t)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (scenario_name))
    :patch-defaults (:default-args (scenario_name research_liability_id)))

(define-entrypoint entity/client/debt_qualifier/au/scenario :implement
  (entity_id scenario_id) (proposed_loan_id)
  :resource (format nil "/entity/client/~A/debt_qualifier/au/scenario/~A" entity_id scenario_id)
  :single-method :post
  :single-resource (format nil "/entity/client/~A/debt_qualifier/au/scenario/~A?_method=implement" entity_id scenario_id))

;; client-v2

(define-dynamiclike-entrypoints (entity/client-v2/debt_qualifier/au/scenario "debt_qualifier/au/scenario" "/entity/client-v2")
    :request-defaults (:list-obj-field scenario_id)
    :put-defaults (:inhibit t)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (scenario_name))
    :patch-defaults (:default-args (scenario_name research_liability_id)))

(define-entrypoint entity/client-v2/debt_qualifier/au/scenario :implement
  (entity_id scenario_id) (proposed_loan_id)
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/au/scenario/~A" entity_id scenario_id)
  :single-method :post
  :single-resource (format nil "/entity/client-v2/~A/debt_qualifier/au/scenario/~A?_method=implement" entity_id scenario_id))

;; client-v3

(define-dynamiclike-entrypoints (entity/client-v3/debt_qualifier/au/scenario "debt_qualifier/au/scenario" "/entity/client-v3")
    :request-defaults (:list-obj-field scenario_id)
    :put-defaults (:inhibit t)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (scenario_name))
    :patch-defaults (:default-args (scenario_name research_liability_id)))

(define-entrypoint entity/client-v3/debt_qualifier/au/scenario :implement
  (entity_id scenario_id) (proposed_loan_id)
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/au/scenario/~A" entity_id scenario_id)
  :single-method :post
  :single-resource (format nil "/entity/client-v3/~A/debt_qualifier/au/scenario/~A?_method=implement" entity_id scenario_id))
