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

File: src/api/debt_qualifier.lisp
Description: /debt_qualifier API Functions
|#

(in-package :cl-xplan-api/api)

;;; debt_qualifier/au/lender_document

;; debt_qualifier/au/lender_document - GET /resourceful/debt_qualifier/au/lender_document/:lender_id
(define-entrypoint debt_qualifier/au/lender_document :get
  (lender_id) () :documentation "Get vector of lender documents for a lender"
  :resource (format nil "/debt_qualifier/au/lender_document/~A" lender_id))

;;; debt_qualifier/au/lender_document_report

;; debt_qualifier/au/lender_document_report - GET /resourceful/debt_qualifier/au/lender_document_report/:document_id
(define-entrypoint debt_qualifier/au/lender_document_report :get
  (document_id) () :documentation "Get a lender document report"
  :resource (format nil "/debt_qualifier/au/lender_document_report/~A" document_id))

;;; debt_qualifier/gb/browse_product

;; debt_qualifier/gb/browse_product - GET /resourceful/debt_qualifier/gb/browse_product
(define-entrypoint debt_qualifier/gb/browse_product :get
  ()
  (((proposed_loan_offset nil offset-p) :cond offset-p :value (if proposed_loan_offset 1 0))
   ((applicant_ftb nil ftb-p) :cond ftb-p :value (if applicant_ftb 1 0))
   property_purchase_type property_value proposed_loan_amount proposed_loan_deposit proposed_loan_term
   proposed_loan_type proposed_loan_repayment_type proposed_loan_init_rate_term
   proposed_loan_remortgage_reason proposed_loan_io_amount lender_group rate_type num_scheme
   sort_field sort_order skip_count)
  :resource "/debt_qualifier/gb/browse_product")

;;; debt_qualifier/gb/credit_history_questions

;; debt_qualifier/gb/credit_history_questions - GET /resourceful/debt_qualifier/gb/credit_history_questions
(define-entrypoint debt_qualifier/gb/credit_history_questions :get
  () () :resource "/debt_qualifier/gb/credit_history_questions"
  :documentation "Get list of predefined credit history questions")

;;; debt_qualifier/gb/custom_panel

;; debt_qualifier/gb/custom_panel - GET /resourceful/debt_qualifier/gb/custom_panel/:entity_id and GET /resourceful/debt_qualifier/gb/custom_panel/:entity_id/:panel_id
(define-entrypoint debt_qualifier/gb/custom_panel :get
  (entity_id panel_id) ((panel_group :cond (and (not panel_id) panel_group)))
  :resource (format nil "GET /resourceful/debt_qualifier/gb/custom_panel/~A~@[/~A~]" entity_id panel_id))  

;; debt_qualifier/gb/custom_panel - POST /resourceful/debt_qualifier/gb/custom_panel/:entity_id
(define-entrypoint debt_qualifier/gb/custom_panel :get
  (entity_id) (panel_name panel_group lender_ids packager_ids)
  :resource (format nil "/debt_qualifier/gb/custom_panel/~A" entity_id))

;; debt_qualifier/gb/custom_panel - PATCH /resourceful/debt_qualifier/gb/custom_panel/:entity_id/:panel_id
(define-entrypoint debt_qualifier/gb/custom_panel :patch (entity_id panel_id)
		   (panel_name panel_group lender_ids packager_ids)
		   :resource (format nil "/debt_qualifier/gb/custom_panel/~A/~A" entity_id panel_id))

;; debt_qualifier/gb/custom_panel - DELETE /resourceful/debt_qualifier/gb/custom_panel/:entity_id/:panel_id
(define-entrypoint debt_qualifier/gb/custom_panel :delete (entity_id panel_id) ()
		   :resource (format nil "/debt_qualifier/gb/custom_panel/~A/~A" entity_id panel_id))

;;; debt_qualifier/gb/kfi_complaint_setting

;; debt_qualifier/gb/kfi_complaint_setting - GET /resourceful/debt_qualifier/gb/kfi_complaint_setting/:group_id
(define-entrypoint debt_qualifier/gb/kfi_complaint_setting :get (group_id) ()
		   :resource (format nil "/debt_qualifier/gb/kfi_complaint_setting/~A" group_id))

;; debt_qualifier/gb/kfi_complaint_setting - PATCH /resourceful/debt_qualifier/gb/kfi_complaint_setting/:group_id
(define-entrypoint debt_qualifier/gb/kfi_complaint_setting :patch (group_id)
		   (block street_name suburb state country postcode department contact url email
			  country_code phone_number)
		   :resource (format nil "/debt_qualifier/gb/kfi_complaint_setting/~A" group_id))

;;; debt_qualifier/gb/panel_provider

;; debt_qualifier/gb/panel_provider - GET /resourceful/debt_qualifier/gb/panel_provider/:panel_group
(define-entrypoint debt_qualifier/gb/panel_provider :get (panel_group) ()
		   :resource (format nil "/debt_qualifier/gb/panel_provider/~A" panel_group)
		   :documentation "Get list of panel providers and packagers for a panel group from Trigold")
