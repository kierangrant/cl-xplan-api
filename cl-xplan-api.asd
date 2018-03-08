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

File: cl-xplan-api.asd
Description: cl-xplan-api ASDF system definition file
|#

(defsystem "cl-xplan-api"
    :version (:read-file-form "src/core/VERSION.expr")
    :author "Kieran Grant"
    :license "LLGPL"
    :description "XPlan API Library"
    :components ((:file "json-patch")
		 (:module
		  "src"
		  :components
		  ((:file "package")
		   (:module
		    "core"
		    :depends-on ("package")
		    :components
		    ((:static-file "VERSION.expr")
		     (:file "definitions")
		     (:file "functions" :depends-on ("definitions"))
		     (:file "macros")
		     (:file "classes")
		     (:file "conditions")
		     (:file "methods" :depends-on ("classes" "conditions" "definitions" "macros"))))
		   (:module
		    "api"
		    :components
		    (;; access
		     (:file "access/client")
		     ;; asset_class
		     (:file "asset_class")
		     ;; assumption_set
		     (:file "assumption_set")
		     (:file "assumption_set/risk_profile")
		     (:file "assumption_set/risk_profile/asset_class")
		     ;; case_manager
		     (:file "case_manager")
		     (:file "case_manager/benchmark")
		     (:file "case_manager/debt_qualifier_scenario")
		     (:file "case_manager/diary")
		     (:file "case_manager/docnote")
		     (:file "case_manager/fee_for_service")
		     (:file "case_manager/fsg")
		     (:file "case_manager/goal")
		     (:file "case_manager/lead")
		     (:file "case_manager/objective")
		     (:file "case_manager/risk_researcher_scenario")
		     (:file "case_manager/savings")
		     (:file "case_manager/supersolver_scenario")
		     (:file "case_manager/task")
		     (:file "case_manager/thread")
		     (:file "case_manager/xtool_scenario")
		     ;; class_smsf
		     (:file "class_smsf/account")
		     (:file "class_smsf/fund")
		     (:file "class_smsf/fund/contribution_caps")
		     (:file "class_smsf/fund/member")
		     (:file "class_smsf/fund_link")
		     ;; client_message
		     (:file "client_message")
		     ;; commission
		     (:file "commission/account/referral")
		     (:file "commission/adviser/account")
		     (:file "commission/client")
		     (:file "commission/control_item")
		     (:file "commission/fee")
		     (:file "commission/lodgement_summary")
		     (:file "commission/policy")
		     (:file "commission/policy/feefacilitation")
		     (:file "commission/product_category")
		     (:file "commission/product")
		     (:file "commission/supplier")
		     (:file "commission/supplier/product")
		     ;; currency
		     (:file "currency")
		     ;; debt_qualifier
		     (:file "debt_qualifier/au/lender_document")
		     (:file "debt_qualifier/au/lender_document_report")
		     (:file "debt_qualifier/gb/broker_fee")
		     (:file "debt_qualifier/gb/browse_product")
		     (:file "debt_qualifier/gb/column_setting/trigold")
		     (:file "debt_qualifier/gb/credit_history_questions")
		     (:file "debt_qualifier/gb/custom_panel")
		     (:file "debt_qualifier/gb/kfi_complaint_setting")
		     (:file "debt_qualifier/gb/panel_provider")
		     ;; digital_signature
		     (:file "digital_signature")
		     (:file "digital_signature/content")
		     (:file "digital_signature/content_path")
		     (:file "digital_signature/finalise")
		     (:file "digital_signature/notify")
		     (:file "digital_signature/signatories")
		     (:file "digital_signature/signatories/generate_code")
		     (:file "digital_signature/signatories/sign")
		     (:file "digital_signature/void")
		     ;; disclaimer
		     (:file "disclaimer/orders")
		     ;; docnote
		     (:file "docnote")
		     (:file "docnote/attachment")
		     (:file "docnote/attachment/content")
		     (:file "docnote/attachment/digital_signature")
		     (:file "docnote/attachment/potential_signatories")
		     (:file "docnote/body")
		     (:file "docnote/case")
		     (:file "docnote/category")
		     (:file "docnote/email")
		     (:file "docnote/firstread")
		     ;; eapplications
		     (:file "eapplications/product")
		     (:file "eapplications/product_type")
		     (:file "eapplications/vendor")
		     (:file "eapplications/vendor/product")
		     (:file "eapplications/vendor/product/option")
		     (:file "eapplications/vendor/service")
		     ;; educational_content
		     (:file "educational_content/category")
		     (:file "educational_content/content")
		     (:file "educational_content/content/body")
		     ;; email
		     (:file "email/email")
		     (:file "email/template")
		     ;; entity
		     (:file "entity/client")
		     (:file "entity/client/@dynamic")
		     (:file "entity/client/@dynamic/attachment")
		     (:file "entity/client/action_to_proceed")
		     (:file "entity/client/address")
		     (:file "entity/client/adviser_identified_needs")
		     (:file "entity/client/annuity")
		     (:file "entity/client/annuity/beneficiary")
		     (:file "entity/client/annuity/owner")
		     (:file "entity/client/annuity/payee")
		     (:file "entity/client/appendices")
		     (:file "entity/client/asset")
		     (:file "entity/client/asset/associate")
		     (:file "entity/client/asset/beneficiary")
		     (:file "entity/client/asset/contribution")
		     (:file "entity/client/asset/investment")
		     (:file "entity/client/asset/liability_link")
		     (:file "entity/client/asset/objective_link")
		     (:file "entity/client/asset/owner")
		     (:file "entity/client/asset/withdrawal")
		     (:file "entity/client/attachment")
		     (:file "entity/client/attorney")
		     (:file "entity/client/calm_entities")
		     (:file "entity/client/capability")
		     (:file "entity/client/cashflow")
		     (:file "entity/client/cashflow/employment")
		     (:file "entity/client/cashflow/owner")
		     (:file "entity/client/cheque_disbursement")
		     (:file "entity/client/client_goals_and_objectives")
		     (:file "entity/client/client_group/member")
		     (:file "entity/client/client_group/member/adviser")
		     (:file "entity/client/client_group/member/adviser/address")
		     (:file "entity/client/client_group/member/adviser/contact")
		     (:file "entity/client/portfolio")
		     (:file "entity/user")
		     ;; error_report
		     (:file "error_report")
		     ;; event
		     (:file "event")
		     ;; family_tree
		     (:file "family_tree")
		     (:file "family_tree/association")
		     (:file "family_tree/dependant")
		     (:file "family_tree/model")
		     (:file "family_tree/owner")
		     (:file "family_tree/person")
		     (:file "family_tree/person/linked_entity")
		     (:file "family_tree/relation")
		     ;; forgot
		     (:file "forgot/username")
		     ;; forgotten_password
		     (:file "forgotten_password")
		     ;; id_verification
		     (:file "id_verification/driver_license")
		     (:file "id_verification/passport")
		     ;; interface
		     (:file "interface")
		     (:file "interface/page")
		     (:file "interface/page/element")
		     (:file "interface/page/element/XiledGroup")
		     (:file "interface/visibility")
		     (:file "interface/wizard")
		     (:file "interface/wizard/behaviour/execute")
		     (:file "interface/wizard/subpage")
		     (:file "interface/wizard/subpage/element")
		     (:file "interface/wizard/subpage/element/XiledGroup")
		     (:file "interface/wizard/subpage/visibility")
		     (:file "interface/wizard/target")
		     ;; job_queue
		     (:file "job_queue/completed_job")
		     ;; knowledge_centre
		     (:file "knowledge_centre/category")
		     (:file "knowledge_centre/category/content/innergi")
		     (:file "knowledge_centre/category/content/innergi/question")
		     (:file "knowledge_centre/category/content/innergi/resource")
		     ;; lead
		     (:file "lead")
		     (:file "lead/case_manager")
		     ;; news
		     (:file "news")
		     ;; online_meeting
		     (:file "online_meeting/meeting")
		     (:file "online_meeting/meeting/attendee")
		     ;; other_allocation
		     (:file "other_allocation")
		     ;; pagination_test
		     (:file "pagination_test")
		     ;; payment_gateway
		     (:file "payment_gateway/worldpay/payment")
		     ;; portfolio
		     (:file "portfolio/position")
		     (:file "portfolio/profit_analysis/detail")
		     (:file "portfolio/transaction")
		     (:file "portfolio/transaction_types")
		     ;; questionnaire
		     (:file "questionnaire")
		     (:file "questionnaire/custom")
		     ;; recaptcha
		     (:file "recaptcha")
		     ;; savings
		     (:file "savings/scenario")
		     ;; session
		     (:file "session/capability")
		     (:file "session/feature_access")
		     (:file "session/transaction")
		     (:file "session/password")
		     (:file "session/password_reset_questions")
		     (:file "session/user")
		     (:file "session/user/adviser")
		     (:file "session/user/adviser/address")
		     (:file "session/user/adviser/contact")
		     (:file "session/user/app_storage")
		     (:file "session/user/assumption_set")
		     (:file "session/user/assumption_set/risk_profile")
		     (:file "session/user/assumption_set/risk_profile/asset_class")
		     ;; site
		     (:file "site")
		     ;; sticky_note
		     (:file "sticky_note")
		     ;; ufield
		     (:file "ufield")
		     (:file "ufield/choice")
		     (:file "ufield/choice_category_dependence"))
		    :depends-on ("core" "package")))
		  :depends-on ("json-patch")))
    :depends-on ("drakma" "cl-json" "babel" "decimals" "cl-base64" "rw-ut" "split-sequence"))
