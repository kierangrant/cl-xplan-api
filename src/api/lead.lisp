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

File: src/api/lead.lisp
Description: /lead API functions
|#

(in-package :cl-xplan-api/api)

;; lead - GET /resourceful/lead and GET /resourceful/lead/:lead_id
(define-entrypoint lead :get
  (lead_id due_date.start_date due_date.end_date modified_date.start_date modified_date.end_date
	   activated_date.start_date activated_date.end_date)
  ((clientid :cond (and clientid (not lead_id)))
   (template :cond (and template (not lead_id)))
   (thread_type :cond (and thread_type (not lead_id)))
   (templateid :cond (and templateid (not lead_id)))
   (templateids :cond (and templateids (not lead_id)))
   (categories :cond (and categories (not lead_id)))
   ((due_date nil due_date-p)
    :cond (and (not lead_id) (or due_date-p due_date.start_date due_date.end_date))
    :value
    (if due_date-p due_date
	(let ((h (make-hash-table)))
	  (if due_date.start_date (setf (gethash "start_date" h) due_date.start_date))
	  (if due_date.end_date (setf (gethash "end_date" h) due_date.end_date))
	  h)))
   ((modified_date nil modified_date-p)
    :cond (and (not lead_id) (or modified_date-p modified_date.start_date modified_date.end_date))
    :value
    (if modified_date-p modified_date
	(let ((h (make-hash-table)))
	  (if modified_date.start_date (setf (gethash "start_date" h) modified_date.start_date))
	  (if modified_date.end_date (setf (gethash "end_date" h) modified_date.end_date))
	  h)))
   ((activated_date nil activated_date-p)
    :cond (and (not lead_id) (or activated_date-p activated_date.start_date activated_date.end_date))
    :value
    (if activated_date-p activated_date
	(let ((h (make-hash-table)))
	  (if activated_date.start_date (setf (gethash "start_date" h) activated_date.start_date))
	  (if activated_date.end_date (setf (gethash "end_date" h) activated_date.end_date))
	  h)))
   (thread_owner :cond (and thread_owner (not lead_id)))
   (lead_stages :cond (and lead_stages (not lead_id)))
   (page :cond (and page (not lead_id)))
   (fields :cond (and lead_id fields)))
  :resource (format nil "/lead~@[/~A~]" lead_id))

;; lead - POST /resourceful/lead
(define-entrypoint lead :post
  (fields.status fields.createdstamp fields.date_due fields.description fields.campaign fields.creator
		 fields.assignee fields.forecast_ongoing fields.referral_entity fields.thread_owner
		 fields.forecast_fua fields.advice_container fields.stage fields.category
		 (fields.likelihood_applied nil fields.likelihood_applied-p) fields.name fields.currency_type
		 fields.likelihood fields.forecast_revenue fields.revenue fields.generated_by fields.source
		 fields.client fields.ongoing fields.template fields.fua fields.actfum fields.from_template
		 (fields.is_joint nil fields.is_joint-p))
  ((fields
    :cond T
    :value
    (if fields
	fields
	(let ((h (make-hash-table)))
	  (if fields.status (setf (gethash "status" h) fields.status))
	  (if fields.createdstamp (setf (gethash "createdstamp" h) fields.createdstamp))
	  (if fields.date_due (setf (gethash "date_due" h) fields.date_due))
	  (if fields.description (setf (gethash "description" h) fields.description))
	  (if fields.campaign (setf (gethash "campaign" h) fields.campaign))
	  (if fields.creator (setf (gethash "creator" h) fields.creator))
	  (if fields.assignee (setf (gethash "assignee" h) fields.assignee))
	  (if fields.forecast_ongoing (setf (gethash "forecast_ongoing" h) fields.forecast_ongoing))
	  (if fields.referral_entity (setf (gethash "referral_entity" h) fields.referral_entity))
	  (if fields.thread_owner (setf (gethash "thread_owner" h) fields.thread_owner))
	  (if fields.forecast_fua (setf (gethash "forecast_fua" h) fields.forecast_fua))
	  (if fields.advice_container (setf (gethash "advice_container" h) fields.advice_container))
	  (if fields.stage (setf (gethash "stage" h) fields.stage))
	  (if fields.category (setf (gethash "category" h) fields.category))
	  (if fields.likelihood_applied-p
	      (setf (gethash "likelihood_applied" h)
		    (if fields.likelihood_applied 1 0)))
	  (if fields.name (setf (gethash "name" h) fields.name))
	  (if fields.currency_type (setf (gethash "currency_type" h) fields.currency_type))
	  (if fields.likelihood (setf (gethash "likelihood" h) fields.likelihood))
	  (if fields.forecast_revenue (setf (gethash "forecast_revenue" h) fields.forecast_revenue))
	  (if fields.revenue (setf (gethash "revenue" h) fields.revenue))
	  (if fields.generated_by (setf (gethash "generated_by" h) fields.generated_by))
	  (if fields.source (setf (gethash "source" h) fields.source))
	  (if fields.client (setf (gethash "client" h) fields.client))
	  (if fields.ongoing (setf (gethash "ongoing" h) fields.ongoing))
	  (if fields.template (setf (gethash "template" h) fields.template))
	  (if fields.fua (setf (gethash "fua" h) fields.fua))
	  (if fields.actfum (setf (gethash "actfum" h) fields.actfum))
	  (if fields.from_template (setf (gethash "from_template" h) fields.from_template))
	  (if fields.is_joint-p (setf (gethash "is_joint" h) (if fields.is_joint 1 0)))
	  h))))
  :resource "/lead")

;; lead - PATCH /resourceful/lead/:lead_id
(define-entrypoint lead :patch
  (lead_id fields.status fields.createdstamp fields.date_due fields.description fields.campaign fields.creator
	   fields.assignee fields.forecast_ongoing fields.referral_entity fields.thread_owner
	   fields.forecast_fua fields.advice_container fields.stage fields.category
	   (fields.likelihood_applied nil fields.likelihood_applied-p) fields.name fields.currency_type
	   fields.likelihood fields.forecast_revenue fields.revenue fields.generated_by fields.source
	   fields.client fields.ongoing fields.template fields.fua fields.actfum fields.from_template
	   (fields.is_joint nil fields.is_joint-p))
  ((fields
    :cond T
    :value
    (if fields
	fields
	(let ((h (make-hash-table)))
	  (if fields.status (setf (gethash "status" h) fields.status))
	  (if fields.createdstamp (setf (gethash "createdstamp" h) fields.createdstamp))
	  (if fields.date_due (setf (gethash "date_due" h) fields.date_due))
	  (if fields.description (setf (gethash "description" h) fields.description))
	  (if fields.campaign (setf (gethash "campaign" h) fields.campaign))
	  (if fields.creator (setf (gethash "creator" h) fields.creator))
	  (if fields.assignee (setf (gethash "assignee" h) fields.assignee))
	  (if fields.forecast_ongoing (setf (gethash "forecast_ongoing" h) fields.forecast_ongoing))
	  (if fields.referral_entity (setf (gethash "referral_entity" h) fields.referral_entity))
	  (if fields.thread_owner (setf (gethash "thread_owner" h) fields.thread_owner))
	  (if fields.forecast_fua (setf (gethash "forecast_fua" h) fields.forecast_fua))
	  (if fields.advice_container (setf (gethash "advice_container" h) fields.advice_container))
	  (if fields.stage (setf (gethash "stage" h) fields.stage))
	  (if fields.category (setf (gethash "category" h) fields.category))
	  (if fields.likelihood_applied-p
	      (setf (gethash "likelihood_applied" h)
		    (if fields.likelihood_applied 1 0)))
	  (if fields.name (setf (gethash "name" h) fields.name))
	  (if fields.currency_type (setf (gethash "currency_type" h) fields.currency_type))
	  (if fields.likelihood (setf (gethash "likelihood" h) fields.likelihood))
	  (if fields.forecast_revenue (setf (gethash "forecast_revenue" h) fields.forecast_revenue))
	  (if fields.revenue (setf (gethash "revenue" h) fields.revenue))
	  (if fields.generated_by (setf (gethash "generated_by" h) fields.generated_by))
	  (if fields.source (setf (gethash "source" h) fields.source))
	  (if fields.client (setf (gethash "client" h) fields.client))
	  (if fields.ongoing (setf (gethash "ongoing" h) fields.ongoing))
	  (if fields.template (setf (gethash "template" h) fields.template))
	  (if fields.fua (setf (gethash "fua" h) fields.fua))
	  (if fields.actfum (setf (gethash "actfum" h) fields.actfum))
	  (if fields.from_template (setf (gethash "from_template" h) fields.from_template))
	  (if fields.is_joint-p (setf (gethash "is_joint" h) (if fields.is_joint 1 0)))
	  h))))
  :resource (format nil "/lead/~A" lead_id)
  :single-parms-as-body T)

;; lead - DELETE /resourceful/lead/:lead_id
(define-entrypoint lead :delete
  (lead_id) ()
  :resource (format nil "/lead/~A" lead_id))
