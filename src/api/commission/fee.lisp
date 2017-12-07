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

File: src/api/commission/fee.lisp
Description: /commission/fee API functions
|#

(in-package :cl-xplan-api/api)

;; commission/fee - POST /resourceful/commission/fee
(define-entrypoint commission/fee :post
  ()
  (due_date
   (product_code :documentation "The product code. To derive the product if product ID not supplied")
   (loan_status_id
    :documentation "mt.tbPolicy.LoanStatusID Including: - Cancelled:'60000000-0000-0000-0000-000003700001' ClaimPaid:'60000000-0000-0000-0000-000003700002' ClaimPending:'60000000-0000-0000-0000-000003700003' Declined:'60000000-0000-0000-0000-000003700004' Deferred:'60000000-0000-0000-0000-000003700005' ExtendedTerm:'60000000-0000-0000-0000-000003700006' ExtendedTermApplied:'60000000-0000-0000-0000-000003700007' Inactive:'60000000-0000-0000-0000-000003700008' Inforce:'60000000-0000-0000-0000-000003700009' Lapsed:'60000000-0000-0000-0000-000003700010' Matured:'60000000-0000-0000-0000-000003700011' NotProceeded:'60000000-0000-0000-0000-000003700012' Other:'60000000-0000-0000-0000-000003700013' PaidUp:'60000000-0000-0000-0000-000003700014' PremiumHoliday:'60000000-0000-0000-0000-000003700015' Recommended:'60000000-0000-0000-0000-000003700016' Surrendered:'60000000-0000-0000-0000-000003700017' UnderClaim:'60000000-0000-0000-0000-000003700018' Underwriting:'60000000-0000-0000-0000-000003700019' Unknown:'60000000-0000-0000-0000-000003700020' Void:'60000000-0000-0000-0000-000003700021'")
   (account_id :documentation "The account id from taccount")
   ((include_tax nil include_tax-p) :cond include_tax-p :value (if include_tax 1 0))
   (description :documentation "Note or description (tpolicypri.description)")
   invoice_date
   (client_original_id :documentation "The xplan id of the client (tClient.OriginalID)")
   (xplan_listobj_index :documentation "The XPLAN ID for the policy")
   (xplan_listobj_type :documentation "The XPLAN listobject type, one of [asset, retirement_fund, insurance_group, insurance, medical_insurance, annuity, feeservice, invoice, fund_za, liability]")
   (partner_original_id :documentation "The xplann id of the partner")
   amount_tax
   (referral_original_id :documentation "The referrer's xplan id (txplanusers.ApplicationUserID)")
   (product_id :documentation "The product id (tProduct.productID)")
   expected_revenue_date
   (policy_status_id :documentation "mt.tbPolicy.PolicyStatusID Lodged:'60000000-0000-0000-0000-0000000f0002' Pending:'60000000-0000-0000-0000-0000000f0001' Locked:'60000000-0000-0000-0000-0000000f0003'")
   amount_net
   (adviser_original_id :documentation "The Adviser's xplan id. To derive the account if account ID not supplied")
   (invoice_number :documentation "The policy number (mt.tbPolicy.policynumber)")
   (account_code :documentation "The adviser account code. To derive the account if account ID not supplied")
   amount_gross
   ((is_joint nil is_joint-p) :cond is_joint-p :value (if is_joint 1 0) :documentation "Is the policy joint"))
  :resource "/commission/fee"
  :documentation "Collection handler for fee")

;; commission/fee - GET /resourceful/commission/fee/:original_id
(define-entrypoint commission/fee :get
  (original_id)
  ((X-XPLAN_AUTHENTICATION :string "X-XPLAN_AUTHENTICATION"
			   :documentation "userId=[user id:int]&siteCode=[siteCode:string]"))
  :resource (format nil "/commission/fee/~A" original_id)
  :documentation "Item handler for fee")

;; commission/fee - PATCH /resourceful/commission/fee/:original_id
(define-entrypoint commission/fee :patch
  (original_id Operations.path Operations.from Operations.value Operations.op)
  (((Operations nil Operations-p) :string "Operations" :cond T
    :value
    (if Operations-p Operations
	(cond-hash
	  (Operations.path "path")
	  (Operations.from "from")
	 (Operations.value "value")
	 (Operations.op "op")))))
  :resource (format nil "/commission/fee/~A" original_id))

;; commission/fee - DELETE /resourceful/commission/fee/:original_id
(define-entrypoint commission/fee :delete
  (original_id)
  ((X-XPLAN_AUTHENTICATION :string "X-XPLAN_AUTHENTICATION"
			   :documentation "userId=[user id:int]&siteCode=[siteCode:string]"))
  :resource (format nil "/commission/fee/~A" original_id))
