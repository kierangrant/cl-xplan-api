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

File: src/api/case_manager.lisp
Description: /case_manager API Functions
|#

(in-package :cl-xplan-api/api)

;;; /case_manager

;; /case_manager - GET /resourceful/case_manager and GET /resourceful/case_manager/:container_id

(cl-xplan-api/core::define-entrypoint case_manager :get
  (container_id)
  ((fields :cond (and container_id fields))
   (clientid :cond (and (not container_id) clientid))
   (template :cond (and (not container_id) template))
   (thread_type :cond (and (not container_id) thread_type))
   (templateid :cond (and (not container_id) templateid))
   (assigneeid :cond (and (not container_id) assigneeid))
   (status :cond (and (not container_id) status))
   (current_past :cond (and (not container_id) current_past))
   (page :cond (and (not container_id) page)))
  :resource (format NIL "/case_manager~@[/~A~]" container_id))

;; /case_manager - POST /resourceful/case_manager

(cl-xplan-api/core::define-entrypoint case_manager :post
  ()
  (fields.status fields.createdstamp fields.date_due fields.description fields.creator fields.fua
		 fields.assignee fields.manual_override_productname fields.locked fields.forecast_fua
		 fields.navigation_flag fields.instigation_type fields.forecast_ongoing
		 fields.category fields.likelihood fields.name fields.advice_container
		 fields.forecast_reveune fields.revenue fields.external_id fields.client
		 fields.ongoing fields.template fields.manual_override_premium fields.scope
		 fields.from_template
		 ((fields.is_joint nil joint-p) :cond joint-p :value (if fields.is_joint 1 0)))
  :resource "/case_manager")

;; /case_manager - PATCH /resourceful/case_manager/:container_id

(cl-xplan-api/core::define-entrypoint case_manager :patch
  (container_id)
  (fields.status fields.createdstamp fields.date_due fields.description fields.creator fields.fua
		 fields.assignee fields.manual_override_productname fields.locked fields.forecast_fua
		 fields.navigation_flag fields.instigation_type fields.forecast_ongoing
		 fields.category fields.likelihood fields.name fields.advice_container
		 fields.forecast_revenue fields.revenue fields.external_id fields.client
		 fields.ongoing fields.template fields.manual_override_premium fields.scope
		 fields.from_template
		 ((fields.is_joint nil joint-p) :cond joint-p :value (if fields.is_joint 1 0)))
  :resource (format NIL "/case_manager/~A" container_id))

;; /case_manager - DELETE /resourceful/case_manager/:container_id

(cl-xplan-api/core::define-entrypoint case_manager :delete
  (container_id)
  ()
  :resource (format NIL "/case_manager/~A" container_id))

;; /case_manager-v2 - GET /resourceful/case_manager-v2

(cl-xplan-api/core::define-entrypoint case_manager-v2 :get
  ()
  (clientid template thread_type templateid assigneeid status current_past page page_size page_sort
	    page_bookmark page_dir)
  :resource "/case_manager-v2")

;; /case_manager-v2 - POST /resourceful/case_manager-v2

(cl-xplan-api/core::define-entrypoint case_manager-v2 :post
  ()
  (fields.status fields.createdstamp fields.date_due fields.description fields.creator fields.fua
		 fields.assignee fields.manual_override_productname fields.locked fields.forecast_fua
		 fields.navigation_flag fields.instigation_type fields.forecast_ongoing
		 fields.category fields.likelihood fields.name fields.advice_container
		 fields.forecast_revenue fields.revenue fields.external_id fields.client
		 fields.ongoing fields.template fields.manual_override_premium fields.scope
		 fields.from_template
		 ((fields.is_joint nil joint-p) :cond joint-p :value (if fields.is_joint 1 0)))
  :resource "/case_manager-v2")

;;; /case_manager/benchmark

;; /case_manager/benchmark - GET /resourceful/case_manager/:container_id/benchmark
(cl-xplan-api/core::define-entrypoint case_manager/benchmark :get (container_id) () :resource (format NIL "/case_manager/~A/benchmark" container_id))

;; /case_manager/debt_qualifier_scenario - GET /resourceful/case_manager/:container_id/debt_qualifier_scenario
(cl-xplan-api/core::define-entrypoint case_manager/debt_qualifier_scenario :get (container_id) () :resource (format NIL "/case_manager/~A/debt_qualifier_scenario" container_id))

;; /case_manager/debt_qualifier_scenario - POST /resourceful/case_manager/:container_id/debt_qualifier_scenario
(cl-xplan-api/core::define-entrypoint case_manager/debt_qualifier_scenario :post
  (container_id) (linked_obj_id ((benchmarkable nil benchmarkable-p) :cond benchmarkable-p :value (if benchmarkable 1 0)))
  :resource (format NIL "/case_manager/~A/debt_qualifier_scenario" container_id))

;; /case_manager/debt_qualifier_scenario - DELETE /resourceful/case_manager/:container_id/debt_qualifier_scenario/:linked_obj_id
(cl-xplan-api/core::define-entrypoint case_manager/debt_qualifier_scenario :delete (container_id linked_obj_id) () :resource (format NIL "/case_manager/~A/debt_qualifier_scenario/~A" container_id linked_obj_id))

;;; case_manager/diary

;; case_manager/diary - GET /resourceful/case_manager/:container_id/diary
(cl-xplan-api/core::define-entrypoint case_manager/diary :get (container_id) () :resource (format NIL "/case_manager/~A/diary" container_id))

;; case_manager/diary - POST /resourceful/case_manager/:container_id/diary
(cl-xplan-api/core::define-entrypoint case_manager/diary :post
  (container_id) (linked_obj_id ((benchmarkable nil benchmarkable-p) :cond benchmarkable-p :value (if benchmarkable 1 0)))
  :resource (format NIL "/case_manager/~A/diary" container_id))

;; case_manager/diary - DELETE /resourceful/case_manager/:container_id/diary/:linked_obj_id
(cl-xplan-api/core::define-entrypoint case_manager/diary :delete (container_id linked_obj_id) () :resource (format NIL "/case_manager/~A/diary/~A" container_id linked_obj_id))

;;; case_manager/docnote

;; case_manager/docnote - GET /resourceful/case_manager/:container_id/docnote
(cl-xplan-api/core::define-entrypoint case_manager/docnote :GET (container_id) () :resource (format nil "/case_manager/~A/docnote" container_id))

;; case_manager/docnote - POST /resourceful/case_manager/:container_id/docnote
(cl-xplan-api/core::define-entrypoint case_manager/docnote :post
  (container_id) (linked_obj_id ((benchmarkable nil benchmarkable-p) :cond benchmarkable-p :value (if benchmarkable 1 0)))
  :resource (format NIL "case_manager/~A/docnote" container_id))

;; case_manager/docnote - DELETE /resourceful/case_manager/:container_id/docnote/:linked_obj_id
(cl-xplan-api/core::define-entrypoint case_manager/docnote :delete (container_id linked_obj_id) () :resource (format NIL "/case_manager/~A/docnote/~A" container_id linked_obj_id))

;;; case_manager/fee_for_service

;; case_manager/fee_for_service - GET /resourceful/case_manager/:container_id/fee_for_service
(cl-xplan-api/core::define-entrypoint case_manager/fee_for_service :GET (container_id) () :resource (format nil "/case_manager/~A/fee_for_service" container_id))

;; case_manager/fee_for_service - POST /resourceful/case_manager/:container_id/fee_for_service
(cl-xplan-api/core::define-entrypoint case_manager/fee_for_service :post
  (container_id) (linked_obj_id ((benchmarkable nil benchmarkable-p) :cond benchmarkable-p :value (if benchmarkable 1 0)))
  :resource (format NIL "case_manager/~A/fee_for_service" container_id))

;; case_manager/fee_for_service - DELETE /resourceful/case_manager/:container_id/fee_for_service/:linked_obj_id
(cl-xplan-api/core::define-entrypoint case_manager/fee_for_service :delete (container_id linked_obj_id) () :resource (format NIL "/case_manager/~A/fee_for_service/~A" container_id linked_obj_id))

;;; case_manager/fsg

;; case_manager/fsg - GET /resourceful/case_manager/:container_id/fsg
(cl-xplan-api/core::define-entrypoint case_manager/fsg :GET (container_id) () :resource (format nil "/case_manager/~A/fsg" container_id))

;; case_manager/fsg - POST /resourceful/case_manager/:container_id/fsg
(cl-xplan-api/core::define-entrypoint case_manager/fsg :post
  (container_id) (linked_obj_id ((benchmarkable nil benchmarkable-p) :cond benchmarkable-p :value (if benchmarkable 1 0)))
  :resource (format NIL "case_manager/~A/fsg" container_id))

;; case_manager/fsg - DELETE /resourceful/case_manager/:container_id/fsg/:linked_obj_id
(cl-xplan-api/core::define-entrypoint case_manager/fsg :delete (container_id linked_obj_id) () :resource (format NIL "/case_manager/~A/fsg/~A" container_id linked_obj_id))

;;; case_manager/goal

;; case_manager/goal - GET /resourceful/case_manager/:container_id/goal
(cl-xplan-api/core::define-entrypoint case_manager/goal :get (container_id) (sort_order) :resource (format NIL "/case_manager/~A/goal" container_id))

;; case_manager/goal - POST /resourceful/case_manager/:container_id/goal
(cl-xplan-api/core::define-entrypoint case_manager/goal :post (container_id) (goal_id entity_id) :resource (format NIL "/case_manager/~A/goal" container_id))

;; case_manager/goal - POST /resourceful/case_manager/:container_id/goal?_method=sort - Unsure if in Bulk I can use 'SORT' method...
(cl-xplan-api/core::define-entrypoint case_manager/goal :sort (container_id) (goals) :resource (format NIL "/case_manager/~A/goal?_method=sort" container_id) :single-method :POST :bulk-method :POST)

;; case_manager/goal - DELETE /resourceful/case_manager/:container_id/goal/:goal_id
(cl-xplan-api/core::define-entrypoint case_manager/goal :delete (container_id goal_id) (entity_id) :resource (format NIL "/case_manager/~A/goal/~A" container_id goal_id))

;;; case_manager/lead

;; case_manager/lead - GET /resourceful/case_manager/:container_id/lead
(cl-xplan-api/core::define-entrypoint case_manager/lead :get (container_id) () :resource (format NIL "/case_manager/~A/lead" container_id))

;; case_manager/lead - POST /resourceful/case_manager/:container_id/lead
(cl-xplan-api/core::define-entrypoint case_manager/lead :post (container_id) (lead_id) :resource (format NIL "/case_manager/~A/lead" container_id))

;; case_manager/lead - DELETE /resourceful/case_manager/:container_id/lead/:lead_id
(cl-xplan-api/core::define-entrypoint case_manager/lead :delete (container_id lead_id) () :resource (format NIL "/case_manager/~A/lead/~A" container_id lead_id))

;;;; ToDo:Add objective, risk_researcher_scenario, savings supersolver_scenario, task, thread, xtool_scenario
