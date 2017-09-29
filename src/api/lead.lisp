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
	(cond-hash
	  (due_date.start_date "start_date")
	  (due_date.end_date "end_date"))))
   ((modified_date nil modified_date-p)
    :cond (and (not lead_id) (or modified_date-p modified_date.start_date modified_date.end_date))
    :value
    (if modified_date-p modified_date
	(cond-hash
	  (modified_date.start_date "start_date")
	  (modified_date.end_date "end_date"))))
   ((activated_date nil activated_date-p)
    :cond (and (not lead_id) (or activated_date-p activated_date.start_date activated_date.end_date))
    :value
    (if activated_date-p activated_date
	(cond-hash
	  (activated_date.start_date "start_date")
	  (activated_date.end_date "end_date"))))
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
	(cond-hash
	  (fields.status "status")
	  (fields.createdstamp "createdstamp")
	  (fields.date_due "date_due")
	  (fields.description "description")
	  (fields.campaign "campaign")
	  (fields.creator "creator")
	  (fields.assignee "assignee")
	  (fields.forecast_ongoing "forecast_ongoing")
	  (fields.referral_entity "referral_entity")
	  (fields.thread_owner "thread_owner")
	  (fields.forecast_fua "forecast_fua")
	  (fields.advice_container "advice_container")
	  (fields.stage "stage")
	  (fields.category "category")
	  (fields.likelihood_applied-p "likelihood_applied" (if fields.likelihood_applied 1 0))
	  (fields.name "name")
	  (fields.currency_type "currency_type")
	  (fields.likelihood "likelihood")
	  (fields.forecast_revenue "forecast_revenue")
	  (fields.revenue "revenue")
	  (fields.generated_by "generated_by")
	  (fields.source "source")
	  (fields.client "client")
	  (fields.ongoing "ongoing")
	  (fields.template "template")
	  (fields.fua "fua")
	  (fields.actfum "actfum")
	  (fields.from_template "from_template")
	  (fields.is_joint-p "is_joint" (if fields.is_joint 1 0))))))
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
	(cond-hash
	  (fields.status "status")
	  (fields.createdstamp "createdstamp")
	  (fields.date_due "date_due")
	  (fields.description "description")
	  (fields.campaign "campaign")
	  (fields.creator "creator")
	  (fields.assignee "assignee")
	  (fields.forecast_ongoing "forecast_ongoing")
	  (fields.referral_entity "referral_entity")
	  (fields.thread_owner "thread_owner")
	  (fields.forecast_fua "forecast_fua")
	  (fields.advice_container "advice_container")
	  (fields.stage "stage")
	  (fields.category "category")
	  (fields.likelihood_applied-p "likelihood_applied" (if fields.likelihood_applied 1 0))
	  (fields.name "name")
	  (fields.currency_type "currency_type")
	  (fields.likelihood "likelihood")
	  (fields.forecast_revenue "forecast_revenue")
	  (fields.revenue "revenue")
	  (fields.generated_by "generated_by")
	  (fields.source "source")
	  (fields.client "client")
	  (fields.ongoing "ongoing")
	  (fields.template "template")
	  (fields.fua "fua")
	  (fields.actfum "actfum")
	  (fields.from_template "from_template")
	  (fields.is_joint-p "is_joint" (if fields.is_joint 1 0))))))
  :resource (format nil "/lead/~A" lead_id)
  :single-parms-as-body T)

;; lead - DELETE /resourceful/lead/:lead_id
(define-entrypoint lead :delete
  (lead_id) ()
  :resource (format nil "/lead/~A" lead_id))
