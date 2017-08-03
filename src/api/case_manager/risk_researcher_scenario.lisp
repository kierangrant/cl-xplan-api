#|
This file is part of CL-XPLAN-API, the Lisp XPLAN API Library

Copyright (C) 2017 Kieran Grant
This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser General Public License
(http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Lisp Lesser GNU General Public License for more details.

File: src/api/case_manager/risk_researcher_scenario.lisp
Description: /case_manager/risk_researcher_scenario API Functions
|#

(in-package :cl-xplan-api/api)

;; case_manager/risk_researcher_scenario - GET /resourceful/case_manager/:container_id/risk_researcher_scenario
(define-entrypoint case_manager/risk_researcher_scenario :get (container_id) ()
		   :resource (format nil "/case_manager/~A/risk_researcher_scenario" container_id))

;; case_manager/risk_researcher_scenario - POST /resourceful/case_manager/:container_id/risk_researcher_scenario
(define-entrypoint case_manager/risk_researcher_scenario :post
  (container_id)
  (linked_obj_id
   ((benchmarkable nil benchmarkable-p) :cond benchmarkable-p :value (if benchmarkable 1 0)))
  :resource (format NIL "/case_manager/~A/risk_researcher_scenario" container_id))

;; case_manager/risk_researcher_scenario - DELETE /resourceful/case_manager/:container_id/risk_researcher_scenario/:linked_obj_id
(define-entrypoint case_manager/risk_researcher_scenario :delete
  (container_id linked_obj_id) ()
  :resource (format NIL "/case_manager/~A/risk_researcher_scenario/~A" container_id linked_obj_id))
