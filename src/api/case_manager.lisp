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

;; case_manager - GET /resourceful/case_manager and GET /resourceful/case_manager/:container_id

(define-entrypoint case_manager :get
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

;; case_manager - POST /resourceful/case_manager

(define-entrypoint case_manager :post
  (status createdstamp date_due description creator fua assignee manual_override_productname locked
	  forecast_fua navigation_flag instigation_type forecast_ongoing category likelihood name
	  advice_container forecast_revenue revenue external_id client ongoing template
	  manual_override_premium scope from_template (is_joint nil joint-p))
  (((fields nil fields-p) :cond T ;; always include, just matter of whether we build it...
    :value
    (if fields-p fields
	(cond-hash
	  (status "status")
	  (createdstamp "createdstamp")
	  (date_due "date_due")
	  (description "description")
	  (creator "creator")
	  (fua "fua")
	  (assignee "assignee")
	  (manual_override_productname "manual_override_productname")
	  (locked "locked")
	  (forecast_fua "forecast_fua")
	  (navigation_flag "navigation_flag")
	  (instigation_type "instigation_type")
	  (forecast_ongoing "forecast_ongoing")
	  (category "category")
	  (likelihood "likelihood")
	  (name "name")
	  (advice_container "advice_container")
	  (forecast_revenue "forecast_revenue")
	  (revenue "revenue")
	  (external_id "external_id")
	  (client "client")
	  (ongoing "ongoing")
	  (template "template")
	  (manual_override_premium "manual_override_premium")
	  (scope "scope")
	  (from_template "from_template")
	  (joint-p "is_joint" (if is_joint 1 0))))))
  :resource "/case_manager"
  :single-parms-as-body T)

;; case_manager - PATCH /resourceful/case_manager/:container_id

(define-entrypoint case_manager :patch

  (container_id status createdstamp date_due description creator fua assignee
		manual_override_productname locked forecast_fua navigation_flag instigation_type
		forecast_ongoing category likelihood name advice_container forecast_revenue revenue
		external_id client ongoing template manual_override_premium scope from_template
		(is_joint nil joint-p))
  (((fields nil fields-p) :cond T ;; always include, just matter of whether we build it...
    :value
    (if fields-p fields
	(cond-hash
	  (status "status")
	  (createdstamp "createdstamp")
	  (date_due "date_due")
	  (description "description")
	  (creator "creator")
	  (fua "fua")
	  (assignee "assignee")
	  (manual_override_productname "manual_override_productname")
	  (locked "locked")
	  (forecast_fua "forecast_fua")
	  (navigation_flag "navigation_flag")
	  (instigation_type "instigation_type")
	  (forecast_ongoing "forecast_ongoing")
	  (category "category")
	  (likelihood "likelihood")
	  (name "name")
	  (advice_container "advice_container")
	  (forecast_revenue "forecast_revenue")
	  (revenue "revenue")
	  (external_id "external_id")
	  (client "client")
	  (ongoing "ongoing")
	  (template "template")
	  (manual_override_premium "manual_override_premium")
	  (scope "scope")
	  (from_template "from_template")
	  (joint-p "is_joint" (if is_joint 1 0))))))
  :single-parms-as-body T
  :resource (format NIL "/case_manager/~A" container_id))

;; case_manager - DELETE /resourceful/case_manager/:container_id

(define-entrypoint case_manager :delete (container_id) ()
		   :resource (format NIL "/case_manager/~A" container_id))

;; case_manager-v2 - GET /resourceful/case_manager-v2

(define-entrypoint case_manager-v2 :get
  ()
  (clientid template thread_type templateid assigneeid status current_past page page_size page_sort
	    page_bookmark page_dir)
  :resource "/case_manager-v2")

;; case_manager-v2 - POST /resourceful/case_manager-v2

(define-entrypoint case_manager-v2 :post
  (status createdstamp date_due description creator fua assignee manual_override_productname locked
	  forecast_fua navigation_flag instigation_type forecast_ongoing category likelihood name
	  advice_container forecast_revenue revenue external_id client ongoing template
	  manual_override_premium scope from_template (is_joint nil joint-p))
  (((fields nil fields-p) :cond T ;; always include, just matter of whether we build it...
    :value
    (if fields-p fields
	(cond-hash
	  (status "status")
	  (createdstamp "createdstamp")
	  (date_due "date_due")
	  (description "description")
	  (creator "creator")
	  (fua "fua")
	  (assignee "assignee")
	  (manual_override_productname "manual_override_productname")
	  (locked "locked")
	  (forecast_fua "forecast_fua")
	  (navigation_flag "navigation_flag")
	  (instigation_type "instigation_type")
	  (forecast_ongoing "forecast_ongoing")
	  (category "category")
	  (likelihood "likelihood")
	  (name "name")
	  (advice_container "advice_container")
	  (forecast_revenue "forecast_revenue")
	  (revenue "revenue")
	  (external_id "external_id")
	  (client "client")
	  (ongoing "ongoing")
	  (template "template")
	  (manual_override_premium "manual_override_premium")
	  (scope "scope")
	  (from_template "from_template")
	  (joint-p "is_joint" (if is_joint 1 0))))))
  :resource "/case_manager-v2"
  :single-parms-as-body T)
