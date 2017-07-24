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

File: src/api/case_manager/xtool_scenario.lisp
Description: /case_manager/xtool_scenario API Functions
|#

(in-package :cl-xplan-api/api)

;; case_manager/xtool_scenario - GET /resourceful/case_manager/:container_id/xtool_scenario
(define-entrypoint case_manager/xtool_scenario :get
  (container_id)
  (((include_frozen nil frozen-p) :cond frozen-p :value (if include_frozen 1 0)))
  :resource (format nil "/case_manager/~A/xtool_scenario" container_id))

;; case_manager/xtool_scenario - POST /resourceful/case_manager/:container_id/xtool_scenario
(define-entrypoint case_manager/xtool_scenario :post
  (container_id) (domain tool_name scenario_name partner_id)
  :resource (format nil "/case_manager/~A/xtool_scenario" container_id))

;; case_manager/xtool_scenario - DELETE /resourceful/case_manager/:container_id/xtool_scenario
(define-entrypoint case_manager/xtool_scenario :delete
  (container_id) (domain tool_name scenario_name partner_id)
  :resource (format nil "/case_manager/~A/xtool_scenario" container_id))
