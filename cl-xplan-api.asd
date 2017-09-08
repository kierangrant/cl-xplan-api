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

File: cl-xplan-api.asd
Description: cl-xplan-api ASDF system definition file
|#

(defsystem :cl-xplan-api
    :version (:read-file-form "src/core/VERSION.expr")
    :author "Kieran Grant"
    :license "LLGPL"
    :description "XPlan API Library"
    :components ((:module
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
		     (:file "digital_signature/notify")
		     (:file "digital_signature/signatories")
		     (:file "digital_signature/signatories/generate_code")
		     (:file "digital_signature/signatories/sign")
		     ;; disclaimer
		     (:file "disclaimer/orders")
		     ;; docnote
		     (:file "docnote")
		     (:file "docnote/attachment")
		     (:file "docnote/attachment/content")
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
		     ;; email
		     (:file "email/email")
		     (:file "email/template")
		     ;; entity
		     (:file "entity/client")
		     (:file "entity/client/cashflow")
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
		     ;; portfolio
		     (:file "portfolio/position")
		     (:file "portfolio/profit_analysis/detail")
		     (:file "portfolio/transaction")
		     (:file "portfolio/transaction_types")
		     ;; session
		     (:file "session/capability")
		     (:file "session/transaction")
		     (:file "session/password")
		     ;; ufield
		     (:file "ufield"))
		    :depends-on ("core" "package")))))
    :depends-on (:drakma :cl-json :babel :decimals :cl-base64 :rw-ut :split-sequence))
