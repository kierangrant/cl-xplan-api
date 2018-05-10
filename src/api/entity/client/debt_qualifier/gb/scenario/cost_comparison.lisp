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

File: src/api/entity/client/debt_qualifier/gb/scenario/cost_comparison.lisp
Description: /entity/client/debt_qualifier/gb/scenario/cost_comparison API functions
|#

(in-package :cl-xplan-api/api)

(define-entrypoint entity/client/debt_qualifier/gb/scenario/cost_comparison :get
  (entity_id scenario_id) ()
  :resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/cost_comparison" entity_id scenario_id))

(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario/cost_comparison :get
  (entity_id scenario_id) ()
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/cost_comparison" entity_id scenario_id))

(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario/cost_comparison :get
  (entity_id scenario_id) ()
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/cost_comparison" entity_id scenario_id))
