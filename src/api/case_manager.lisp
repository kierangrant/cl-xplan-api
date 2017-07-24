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
	(let ((h (make-hash-table)))
	  (if status (setf (gethash "status" h) status))
	  (if createdstamp (setf (gethash "createdstamp" h) createdstamp))
	  (if date_due (setf (gethash "date_due" h) date_due))
	  (if description (setf (gethash "description" h) description))
	  (if creator (setf (gethash "creator" h) creator))
	  (if fua (setf (gethash "fua" h) fua))
	  (if assignee (setf (gethash "assignee" h) assignee))
	  (if manual_override_productname
	      (setf (gethash "manual_override_productname" h) manual_override_productname))
	  (if locked (setf (gethash "locked" h) locked))
	  (if forecast_fua (setf (gethash "forecast_fua" h) forecast_fua))
	  (if navigation_flag (setf (gethash "navigation_flag" h) navigation_flag))
	  (if instigation_type (setf (gethash "instigation_type" h) instigation_type))
	  (if forecast_ongoing (setf (gethash "forecast_ongoing" h) forecast_ongoing))
	  (if category (setf (gethash "category" h) category))
	  (if likelihood (setf (gethash "likelihood" h) likelihood))
	  (if name (setf (gethash "name" h) name))
	  (if advice_container (setf (gethash "advice_container" h) advice_container))
	  (if forecast_revenue (setf (gethash "forecast_revenue" h) forecast_revenue))
	  (if revenue (setf (gethash "revenue" h) revenue))
	  (if external_id (setf (gethash "external_id" h) external_id))
	  (if client (setf (gethash "client" h) client))
	  (if ongoing (setf (gethash "ongoing" h) ongoing))
	  (if template (setf (gethash "template" h) template))
	  (if manual_override_premium (setf (gethash "manual_override_premium" h) manual_override_premium))
	  (if scope (setf (gethash "scope" h) scope))
	  (if from_template (setf (gethash "from_template" h) from_template))
	  (if joint-p (setf (gethash "is_joint" h) (if is_joint 1 0)))
	  h))))
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
	(let ((h (make-hash-table)))
	  (if status (setf (gethash "status" h) status))
	  (if createdstamp (setf (gethash "createdstamp" h) createdstamp))
	  (if date_due (setf (gethash "date_due" h) date_due))
	  (if description (setf (gethash "description" h) description))
	  (if creator (setf (gethash "creator" h) creator))
	  (if fua (setf (gethash "fua" h) fua))
	  (if assignee (setf (gethash "assignee" h) assignee))
	  (if manual_override_productname
	      (setf (gethash "manual_override_productname" h) manual_override_productname))
	  (if locked (setf (gethash "locked" h) locked))
	  (if forecast_fua (setf (gethash "forecast_fua" h) forecast_fua))
	  (if navigation_flag (setf (gethash "navigation_flag" h) navigation_flag))
	  (if instigation_type (setf (gethash "instigation_type" h) instigation_type))
	  (if forecast_ongoing (setf (gethash "forecast_ongoing" h) forecast_ongoing))
	  (if category (setf (gethash "category" h) category))
	  (if likelihood (setf (gethash "likelihood" h) likelihood))
	  (if name (setf (gethash "name" h) name))
	  (if advice_container (setf (gethash "advice_container" h) advice_container))
	  (if forecast_revenue (setf (gethash "forecast_revenue" h) forecast_revenue))
	  (if revenue (setf (gethash "revenue" h) revenue))
	  (if external_id (setf (gethash "external_id" h) external_id))
	  (if client (setf (gethash "client" h) client))
	  (if ongoing (setf (gethash "ongoing" h) ongoing))
	  (if template (setf (gethash "template" h) template))
	  (if manual_override_premium (setf (gethash "manual_override_premium" h) manual_override_premium))
	  (if scope (setf (gethash "scope" h) scope))
	  (if from_template (setf (gethash "from_template" h) from_template))
	  (if joint-p (setf (gethash "is_joint" h) (if is_joint 1 0)))
	  h))))
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
	(let ((h (make-hash-table)))
	  (if status (setf (gethash "status" h) status))
	  (if createdstamp (setf (gethash "createdstamp" h) createdstamp))
	  (if date_due (setf (gethash "date_due" h) date_due))
	  (if description (setf (gethash "description" h) description))
	  (if creator (setf (gethash "creator" h) creator))
	  (if fua (setf (gethash "fua" h) fua))
	  (if assignee (setf (gethash "assignee" h) assignee))
	  (if manual_override_productname
	      (setf (gethash "manual_override_productname" h) manual_override_productname))
	  (if locked (setf (gethash "locked" h) locked))
	  (if forecast_fua (setf (gethash "forecast_fua" h) forecast_fua))
	  (if navigation_flag (setf (gethash "navigation_flag" h) navigation_flag))
	  (if instigation_type (setf (gethash "instigation_type" h) instigation_type))
	  (if forecast_ongoing (setf (gethash "forecast_ongoing" h) forecast_ongoing))
	  (if category (setf (gethash "category" h) category))
	  (if likelihood (setf (gethash "likelihood" h) likelihood))
	  (if name (setf (gethash "name" h) name))
	  (if advice_container (setf (gethash "advice_container" h) advice_container))
	  (if forecast_revenue (setf (gethash "forecast_revenue" h) forecast_revenue))
	  (if revenue (setf (gethash "revenue" h) revenue))
	  (if external_id (setf (gethash "external_id" h) external_id))
	  (if client (setf (gethash "client" h) client))
	  (if ongoing (setf (gethash "ongoing" h) ongoing))
	  (if template (setf (gethash "template" h) template))
	  (if manual_override_premium (setf (gethash "manual_override_premium" h) manual_override_premium))
	  (if scope (setf (gethash "scope" h) scope))
	  (if from_template (setf (gethash "from_template" h) from_template))
	  (if joint-p (setf (gethash "is_joint" h) (if is_joint 1 0)))
	  h))))
  :resource "/case_manager-v2"
  :single-parms-as-body T)
