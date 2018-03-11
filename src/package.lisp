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

File: src/package.lisp
Description: Package definition for CL-XPLAN-API
|#

(in-package :cl-user)

(defpackage :cl-xplan-api/core
  (:use :cl)
  (:export
   ;; classes
   #:xplan-request
   #:xplan-request-bulk
   #:xplan-request-bulk-requests
   #:xplan-session
   ;; conditions
   #:xplan-api-error
   ;; methods
   #:api-key
   #:base-url
   #:content
   #:content-type
   #:delete-session
   #:drakma-settings
   #:force-init-auth
   #:name
   #:password
   #:prepare-request
   #:process-request
   #:parameters
   #:transport-version
   #:response
   #:response-code
   #:response-msg
   #:response-headers
   #:response-time
   #:resource
   #:requests
   #:request-method
   #:username
   #:user-agent
   #:xplan-api-call
   #:xplan-api-error-status-code
   #:xplan-api-error-reason-message
   #:xplan-api-error-request
   #:xplan-session-auto-reauth
   #:get-request-by-name
   ;; functions
   #:convert-xplan-type-to-native
   #:convert-native-to-xplan-type
   #:convert-bulk-to-native
   ;; macros
   #:with-xplan-session
   #:with-bulk-request
   ;; variables
   *max-rounding*
   *xplan-api-debug*
   )
  (:intern "DEFINE-ENTRYPOINT" "COND-HASH" "DEFINE-DYNAMICLIKE-ENTRYPOINTS"))

(defpackage :cl-xplan-api/api
  (:use :cl)
  (:import-from :cl-xplan-api/core #:define-entrypoint #:cond-hash #:define-dynamiclike-entrypoints)
  (:export
   ;; access
   #:access/client
   ;; asset_class
   #:asset_class
   ;; assumption_set
   #:assumption_set
   #:assumption_set/risk_profile
   #:assumption_set/risk_profile/asset_class
   ;; case_manager
   #:case_manager
   #:case_manager-v2
   #:case_manager/benchmark
   #:case_manager/debt_qualifier_scenario
   #:case_manager/diary
   #:case_manager/docnote
   #:case_manager/fee_for_service
   #:case_manager/fsg
   #:case_manager/goal
   #:case_manager/lead
   #:case_manager/objective
   #:case_manager/risk_researcher_scenario
   #:case_manager/savings
   #:case_manager/supersolver_scenario
   #:case_manager/task
   #:case_manager/thread
   #:case_manager/xtool_scenario
   ;; class_smsf
   #:class_smsf/account
   #:class_smsf/fund
   #:class_smsf/fund/contribution_caps
   #:class_smsf/fund/members
   #:class_smsf/fund_link
   ;; client_message
   #:client_message
   ;; commission
   #:commission/account/referral
   #:commission/adviser/account
   #:commission/client
   #:commission/control_item
   #:commission/fee
   #:commission/lodgement_summary
   #:commission/policy
   #:commission/policy/feefacilitation
   #:commission/product_category
   #:commission/product
   #:commission/supplier
   #:commission/supplier/product
   ;; currency
   #:currency
   #:currency-v2
   ;; debt_qualifier
   #:debt_qualifier/au/lender_document
   #:debt_qualifier/au/lender_document_report
   #:debt_qualifier/gb/broker_fee
   #:debt_qualifier/gb/browse_product
   #:debt_qualifier/gb/column_setting/trigold
   #:debt_qualifier/gb/credit_history_questions
   #:debt_qualifier/gb/custom_panel
   #:debt_qualifier/gb/kfi_complaint_setting
   #:debt_qualifier/gb/panel_provider
   ;; digital_signature
   #:digital_signature
   #:digital_signature/content
   #:digital_signature/content_path
   #:digital_signature/finalise
   #:digital_signature/notify
   #:digital_signature/signatories
   #:digital_signature/signatories/generate_code
   #:digital_signature/signatories/sign
   #:digital_signature/void
   ;; disclaimer
   #:disclaimer/orders
   ;; docnote
   #:docnote
   #:docnote-v2
   #:docnote/attachment
   #:docnote-v2/attachment
   #:docnote/attachment-v2
   #:docnote-v2/attachment-v2
   #:docnote/attachment-v3
   #:docnote-v2/attachment-v3
   #:docnote/attachment-v3/content
   #:docnote-v2/attachment-v3/content
   #:docnote/attachment-v3/digital_signature
   #:docnote-v2/attachment-v3/digital_signature
   #:docnote/attachment-v3/potential_signatories
   #:docnote-v2/attachment-v3/potential_signatories
   #:docnote-v2/body
   #:docnote-v2/case
   #:docnote/category
   #:docnote-v2/category
   #:docnote-v2/email
   #:docnote-v2/firstread
   ;; eapplications
   #:eapplications/product
   #:eapplications/product_type
   #:eapplications/vendor
   #:eapplications/vendor/product
   #:eapplications/vendor/product/option
   #:eapplications/vendor/service
   ;; educational_content
   #:educational_content/category
   #:educational_content/content
   #:educational_content/content/body
   ;; email
   #:email/email
   #:email/template
   ;; entity
   #:entity/client
   #:entity/client-v2
   #:entity/client-v3
   #:entity/client/<dynamic>
   #:entity/client-v2/<dynamic>
   #:entity/client-v3/<dynamic>
   #:entity/client/<dynamic>/attachment
   #:entity/client-v2/<dynamic>/attachment
   #:entity/client-v3/<dynamic>/attachment
   #:entity/client/action_to_proceed
   #:entity/client-v2/action_to_proceed
   #:entity/client-v3/action_to_proceed
   #:entity/client/address
   #:entity/client-v2/address
   #:entity/client-v3/address
   #:entity/client/adviser_identified_needs
   #:entity/client-v2/adviser_identified_needs
   #:entity/client-v3/adviser_identified_needs
   #:entity/client/annuity
   #:entity/client-v2/annuity
   #:entity/client-v3/annuity
   #:entity/client/annuity/beneficiary
   #:entity/client-v2/annuity/beneficiary
   #:entity/client-v3/annuity/beneficiary
   #:entity/client/annuity/owner
   #:entity/client-v2/annuity/owner
   #:entity/client-v3/annuity/owner
   #:entity/client/annuity/payee
   #:entity/client-v2/annuity/payee
   #:entity/client-v3/annuity/payee
   #:entity/client/appendices
   #:entity/client-v2/appendices
   #:entity/client-v3/appendices
   #:entity/client/asset
   #:entity/client-v2/asset
   #:entity/client-v3/asset
   #:entity/client/asset/associate
   #:entity/client-v2/asset/associate
   #:entity/client-v3/asset/associate
   #:entity/client/asset/beneficiary
   #:entity/client-v2/asset/beneficiary
   #:entity/client-v3/asset/beneficiary
   #:entity/client/asset/contribution
   #:entity/client-v2/asset/contribution
   #:entity/client-v3/asset/contribution
   #:entity/client/asset/investment
   #:entity/client-v2/asset/investment
   #:entity/client-v3/asset/investment
   #:entity/client/asset/liability_link
   #:entity/client-v2/asset/liability_link
   #:entity/client-v3/asset/liability_link
   #:entity/client/asset/objective_link
   #:entity/client-v2/asset/objective_link
   #:entity/client-v3/asset/objective_link
   #:entity/client/asset/owner
   #:entity/client-v2/asset/owner
   #:entity/client-v3/asset/owner
   #:entity/client/asset/withdrawal
   #:entity/client-v2/asset/withdrawal
   #:entity/client-v3/asset/withdrawal
   #:entity/client/attachment
   #:entity/client-v2/attachment
   #:entity/client-v3/attachment
   #:entity/client/attorney
   #:entity/client-v2/attorney
   #:entity/client-v3/attorney
   #:entity/client/calm_entities
   #:entity/client-v2/calm_entities
   #:entity/client-v3/calm_entities
   #:entity/client/capability
   #:entity/client-v2/capability
   #:entity/client-v3/capability
   #:entity/client/portfolio
   #:entity/client-v2/portfolio
   #:entity/client-v3/portfolio
   #:entity/client/cashflow
   #:entity/client-v2/cashflow
   #:entity/client-v3/cashflow
   #:entity/client/cashflow/employment
   #:entity/client-v2/cashflow/employment
   #:entity/client-v3/cashflow/employment
   #:entity/client/cashflow/owner
   #:entity/client-v2/cashflow/owner
   #:entity/client-v3/cashflow/owner
   #:entity/client/cheque_disbursement
   #:entity/client-v2/cheque_disbursement
   #:entity/client-v3/cheque_disbursement
   #:entity/client/client_goals_and_objectives
   #:entity/client-v2/client_goals_and_objectives
   #:entity/client-v3/client_goals_and_objectives
   #:entity/client/client_group/member
   #:entity/client-v2/client_group/member
   #:entity/client-v3/client_group/member
   #:entity/client/client_group/member/adviser
   #:entity/client-v2/client_group/member/adviser
   #:entity/client-v3/client_group/member/adviser
   #:entity/client/client_group/member/adviser/address
   #:entity/client-v2/client_group/member/adviser/address
   #:entity/client-v3/client_group/member/adviser/address
   #:entity/client/client_group/member/adviser/contact
   #:entity/client-v2/client_group/member/adviser/contact
   #:entity/client-v3/client_group/member/adviser/contact
   #:entity/client/company_keyperson
   #:entity/client-v2/company_keyperson
   #:entity/client-v3/company_keyperson
   #:entity/client/contact
   #:entity/client-v2/contact
   #:entity/client-v3/contact
   #:entity/user
   #:entity/user-v2
   ;; error_report
   #:error_report
   ;; event
   #:event
   #:event-v2
   ;; family_tree
   #:family_tree
   #:family_tree/association
   #:family_tree/dependant
   #:family_tree/model
   #:family_tree/owner
   #:family_tree/person
   #:family_tree/person-v2
   #:family_tree/person/linked_entity
   #:family_tree/person-v2/linked_entity
   #:family_tree/relation
   ;; forgot
   #:forgot/username
   ;; forgotten_password
   #:forgotten_password
   ;; id_verification
   #:id_verification/driver_license
   #:id_verification/passport
   ;; interface
   #:interface
   #:interface/page
   #:interface/page/element
   #:interface/page/element/XiledGroup
   #:interface/visibility
   #:interface/wizard
   #:interface/wizard/behaviour/execute
   #:interface/wizard/subpage
   #:interface/wizard/subpage/element
   #:interface/wizard/subpage/element/XiledGroup
   #:interface/wizard/subpage/visibility
   #:interface/wizard/target
   ;; job_queue
   #:job_queue/completed_job
   ;; knowledge_centre
   #:knowledge_centre/category
   #:knowledge_centre/category/content/innergi
   #:knowledge_centre/category/content/innergi/question
   #:knowledge_centre/category/content/innergi/resource
   ;; lead
   #:lead
   #:lead/case_manager
   ;; news
   #:news
   ;; online_meeting
   #:online_meeting/meeting
   #:online_meeting/meeting/attendee
   ;; other_allocation
   #:other_allocation
   ;; pagination_test
   #:pagination_test
   ;; payment_gateway
   #:payment_gateway/worldpay/payment
   ;; portfolio
   #:portfolio/position
   #:portfolio/profit_analysis/detail
   #:portfolio/transaction
   #:portfolio/transaction_types
   ;; questionnaire
   #:questionnaire
   #:questionnaire/custom
   ;; recaptcha
   #:recaptcha
   ;; savings
   #:savings/scenario
   ;; session
   #:session/capability
   #:session/feature_access
   #:session/password
   #:session/password_reset_questions
   #:session/transaction
   #:session/user
   #:session/user/adviser
   #:session/user/adviser/address
   #:session/user/adviser/contact
   #:session/user/app_storage
   #:session/user/assumption_set
   #:session/user/assumption_set/risk_profile
   #:session/user/assumption_set/risk_profile/asset_class
   ;; site
   #:site
   ;; sticky_note
   #:sticky_note
   ;; ufield
   #:ufield
   #:ufield/choice
   #:ufield/choice_category_dependence
   ))

(defpackage :cl-xplan-api
  (:use :cl :cl-xplan-api/core :cl-xplan-api/api)
  (:export
   ;; export useful items from xplan-api
   #:xplan-session
   #:username
   #:password
   #:base-url
   #:api-key
   #:with-xplan-session
   #:with-bulk-request
   #:get-request-by-name
   #:process-request
   #:delete-session
   #:convert-xplan-type-to-native
   #:convert-native-to-xplan-type
   #:convert-bulk-to-native
   #:response
   #:response-time
   ;; export API
   ;; access
   #:access/client
   ;; asset_class
   #:asset_class
   ;; assumption_set
   #:assumption_set
   #:assumption_set/risk_profile
   #:assumption_set/risk_profile/asset_class
   ;; case_manager
   #:case_manager
   #:case_manager-v2
   #:case_manager/benchmark
   #:case_manager/debt_qualifier_scenario
   #:case_manager/diary
   #:case_manager/docnote
   #:case_manager/fee_for_service
   #:case_manager/fsg
   #:case_manager/goal
   #:case_manager/lead
   #:case_manager/objective
   #:case_manager/risk_researcher_scenario
   #:case_manager/savings
   #:case_manager/supersolver_scenario
   #:case_manager/task
   #:case_manager/thread
   #:case_manager/xtool_scenario
   ;; class_smsf
   #:class_smsf/account
   #:class_smsf/fund
   #:class_smsf/fund/contribution_caps
   #:class_smsf/fund/members
   #:class_smsf/fund_link
   ;; client_message
   #:client_message
   ;; commission
   #:commission/account/referral
   #:commission/adviser/account
   #:commission/client
   #:commission/control_item
   #:commission/fee
   #:commission/lodgement_summary
   #:commission/policy
   #:commission/policy/feefacilitation
   #:commission/product_category
   #:commission/product
   #:commission/supplier
   #:commission/supplier/product
   ;; currency
   #:currency
   #:currency-v2
   ;; debt_qualifier
   #:debt_qualifier/au/lender_document
   #:debt_qualifier/au/lender_document_report
   #:debt_qualifier/gb/broker_fee
   #:debt_qualifier/gb/browse_product
   #:debt_qualifier/gb/column_setting/trigold
   #:debt_qualifier/gb/credit_history_questions
   #:debt_qualifier/gb/custom_panel
   #:debt_qualifier/gb/kfi_complaint_setting
   #:debt_qualifier/gb/panel_provider
   ;; digital_signature
   #:digital_signature
   #:digital_signature/content
   #:digital_signature/content_path
   #:digital_signature/finalise
   #:digital_signature/notify
   #:digital_signature/signatories
   #:digital_signature/signatories/generate_code
   #:digital_signature/signatories/sign
   #:digital_signature/void
   ;; disclaimer
   #:disclaimer/orders
   ;; docnote
   #:docnote
   #:docnote-v2
   #:docnote/attachment
   #:docnote-v2/attachment
   #:docnote/attachment-v2
   #:docnote-v2/attachment-v2
   #:docnote/attachment-v3
   #:docnote-v2/attachment-v3
   #:docnote/attachment-v3/content
   #:docnote-v2/attachment-v3/content
   #:docnote/attachment-v3/digital_signature
   #:docnote-v2/attachment-v3/digital_signature
   #:docnote/attachment-v3/potential_signatories
   #:docnote-v2/attachment-v3/potential_signatories
   #:docnote-v2/body
   #:docnote-v2/case
   #:docnote/category
   #:docnote-v2/category
   #:docnote-v2/email
   #:docnote-v2/firstread
   ;; eapplications
   #:eapplications/product
   #:eapplications/product_type
   #:eapplications/vendor
   #:eapplications/vendor/product
   #:eapplications/vendor/product/option
   #:eapplications/vendor/service
   ;; educational_content
   #:educational_content/category
   #:educational_content/content
   #:educational_content/content/body
   ;; email
   #:email/email
   #:email/template
   ;; entity
   #:entity/client
   #:entity/client-v2
   #:entity/client-v3
   #:entity/client/<dynamic>
   #:entity/client-v2/<dynamic>
   #:entity/client-v3/<dynamic>
   #:entity/client/<dynamic>/attachment
   #:entity/client-v2/<dynamic>/attachment
   #:entity/client-v3/<dynamic>/attachment
   #:entity/client/action_to_proceed
   #:entity/client-v2/action_to_proceed
   #:entity/client-v3/action_to_proceed
   #:entity/client/adviser_identified_needs
   #:entity/client-v2/adviser_identified_needs
   #:entity/client-v3/adviser_identified_needs
   #:entity/client/address
   #:entity/client-v2/address
   #:entity/client-v3/address
   #:entity/client/annuity
   #:entity/client-v2/annuity
   #:entity/client-v3/annuity
   #:entity/client/annuity/beneficiary
   #:entity/client-v2/annuity/beneficiary
   #:entity/client-v3/annuity/beneficiary
   #:entity/client/annuity/owner
   #:entity/client-v2/annuity/owner
   #:entity/client-v3/annuity/owner
   #:entity/client/annuity/payee
   #:entity/client-v2/annuity/payee
   #:entity/client-v3/annuity/payee
   #:entity/client/appendices
   #:entity/client-v2/appendices
   #:entity/client-v3/appendices
   #:entity/client/asset
   #:entity/client-v2/asset
   #:entity/client-v3/asset
   #:entity/client/asset/associate
   #:entity/client-v2/asset/associate
   #:entity/client-v3/asset/associate
   #:entity/client/asset/beneficiary
   #:entity/client-v2/asset/beneficiary
   #:entity/client-v3/asset/beneficiary
   #:entity/client/asset/contribution
   #:entity/client-v2/asset/contribution
   #:entity/client-v3/asset/contribution
   #:entity/client/asset/investment
   #:entity/client-v2/asset/investment
   #:entity/client-v3/asset/investment
   #:entity/client/asset/liability_link
   #:entity/client-v2/asset/liability_link
   #:entity/client-v3/asset/liability_link
   #:entity/client/asset/objective_link
   #:entity/client-v2/asset/objective_link
   #:entity/client-v3/asset/objective_link
   #:entity/client/asset/owner
   #:entity/client-v2/asset/owner
   #:entity/client-v3/asset/owner
   #:entity/client/asset/withdrawal
   #:entity/client-v2/asset/withdrawal
   #:entity/client-v3/asset/withdrawal
   #:entity/client/attachment
   #:entity/client-v2/attachment
   #:entity/client-v3/attachment
   #:entity/client/attorney
   #:entity/client-v2/attorney
   #:entity/client-v3/attorney
   #:entity/client/calm_entities
   #:entity/client-v2/calm_entities
   #:entity/client-v3/calm_entities
   #:entity/client/capability
   #:entity/client-v2/capability
   #:entity/client-v3/capability
   #:entity/client/portfolio
   #:entity/client-v2/portfolio
   #:entity/client-v3/portfolio
   #:entity/client/cashflow
   #:entity/client-v2/cashflow
   #:entity/client-v3/cashflow
   #:entity/client/cashflow/employment
   #:entity/client-v2/cashflow/employment
   #:entity/client-v3/cashflow/employment
   #:entity/client/cashflow/owner
   #:entity/client-v2/cashflow/owner
   #:entity/client-v3/cashflow/owner
   #:entity/client/cheque_disbursement
   #:entity/client-v2/cheque_disbursement
   #:entity/client-v3/cheque_disbursement
   #:entity/client/client_goals_and_objectives
   #:entity/client-v2/client_goals_and_objectives
   #:entity/client-v3/client_goals_and_objectives
   #:entity/client/client_group/member
   #:entity/client-v2/client_group/member
   #:entity/client-v3/client_group/member
   #:entity/client/client_group/member/adviser
   #:entity/client-v2/client_group/member/adviser
   #:entity/client-v3/client_group/member/adviser
   #:entity/client/client_group/member/adviser/address
   #:entity/client-v2/client_group/member/adviser/address
   #:entity/client-v3/client_group/member/adviser/address
   #:entity/client/client_group/member/adviser/contact
   #:entity/client-v2/client_group/member/adviser/contact
   #:entity/client-v3/client_group/member/adviser/contact
   #:entity/client/company_keyperson
   #:entity/client-v2/company_keyperson
   #:entity/client-v3/company_keyperson
   #:entity/client/contact
   #:entity/client-v2/contact
   #:entity/client-v3/contact
   #:entity/user
   #:entity/user-v2
   ;; error_report
   #:error_report
   ;; event
   #:event
   #:event-v2
   ;; family_tree
   #:family_tree
   #:family_tree/association
   #:family_tree/dependant
   #:family_tree/model
   #:family_tree/owner
   #:family_tree/person
   #:family_tree/person-v2
   #:family_tree/person/linked_entity
   #:family_tree/person-v2/linked_entity
   #:family_tree/relation
   ;; forgot
   #:forgot/username
   ;; forgotten_password
   #:forgotten_password
   ;; id_verification
   #:id_verification/driver_license
   #:id_verification/passport
   ;; interface
   #:interface
   #:interface/page
   #:interface/page/element
   #:interface/page/element/XiledGroup
   #:interface/visibility
   #:interface/wizard
   #:interface/wizard/behaviour/execute
   #:interface/wizard/subpage
   #:interface/wizard/subpage/element
   #:interface/wizard/subpage/element/XiledGroup
   #:interface/wizard/subpage/visibility
   #:interface/wizard/target
   ;; job_queue
   #:job_queue/completed_job
   ;; knowledge_centre
   #:knowledge_centre/category
   #:knowledge_centre/category/content/innergi
   #:knowledge_centre/category/content/innergi/question
   #:knowledge_centre/category/content/innergi/resource
   ;; lead
   #:lead
   #:lead/case_manager
   ;; news
   #:news
   ;; online_meeting
   #:online_meeting/meeting
   #:online_meeting/meeting/attendee
   ;; other_allocation
   #:other_allocation
   ;; pagination_test
   #:pagination_test
   ;; payment_gateway
   #:payment_gateway/worldpay/payment
   ;; portfolio
   #:portfolio/position
   #:portfolio/profit_analysis/detail
   #:portfolio/transaction
   #:portfolio/transaction_types
   ;; questionnaire
   #:questionnaire
   #:questionnaire/custom
   ;; recaptcha
   #:recaptcha
   ;; savings
   #:savings/scenario
   ;; session
   #:session/capability
   #:session/feature_access
   #:session/password
   #:session/password_reset_questions
   #:session/transaction
   #:session/user
   #:session/user/adviser
   #:session/user/adviser/address
   #:session/user/adviser/contact
   #:session/user/app_storage
   #:session/user/assumption_set
   #:session/user/assumption_set/risk_profile
   #:session/user/assumption_set/risk_profile/asset_class
   ;; site
   #:site
   ;; sticky_note
   #:sticky_note
   ;; ufield
   #:ufield
   #:ufield/choice
   #:ufield/choice_category_dependence
   ))
