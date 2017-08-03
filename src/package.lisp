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

File: src/package.lisp
Description: Package definition for CL-XPLAN-API
|#

(in-package :cl-user)

(defpackage :cl-xplan-api/core
  (:use :cl)
  (:export
   ;; classes
   xplan-request
   xplan-request-bulk
   xplan-request-bulk-requests
   xplan-session
   ;; conditions
   xplan-api-error
   ;; methods
   api-key
   base-url
   content
   content-type
   delete-session
   drakma-settings
   force-init-auth
   name
   password
   prepare-request
   process-request
   parameters
   transport-version
   response
   response-code
   response-msg
   response-headers
   response-time
   resource
   requests
   request-method
   username
   user-agent
   xplan-api-call
   xplan-api-error-status-code
   xplan-api-error-reason-message
   xplan-api-error-request
   xplan-session-auto-reauth
   get-request-by-name
   ;; functions
   convert-xplan-type-to-native
   convert-native-to-xplan-type
   convert-bulk-to-native
   ;; macros
   with-xplan-session
   with-bulk-request
   define-entrypoint
   ;; variables
   *max-rounding*
   *xplan-api-debug*
   ))

(defpackage :cl-xplan-api/api
  (:use :cl)
  (:import-from :cl-xplan-api/core :define-entrypoint)
  (:export
   ;; access
   access/client
   ;; asset_class
   asset_class
   ;; assumption_set
   assumption_set
   assumption_set/risk_profile
   assumption_set/risk_profile/asset_class
   ;; case_manager
   case_manager
   case_manager-v2
   case_manager/benchmark
   case_manager/debt_qualifier_scenario
   case_manager/diary
   case_manager/docnote
   case_manager/fee_for_service
   case_manager/fsg
   case_manager/goal
   case_manager/lead
   case_manager/objective
   case_manager/risk_researcher_scenario
   case_manager/savings
   case_manager/supersolver_scenario
   case_manager/task
   case_manager/thread
   case_manager/xtool_scenario
   ;; class_smsf
   class_smsf/account
   class_smsf/fund
   class_smsf/fund/contribution_caps
   class_smsf/fund/members
   class_smsf/fund_link
   ;; client_message
   client_message
   ;; currency
   currency
   currency-v2
   ;; debt_qualifier
   debt_qualifier/au/lender_document
   debt_qualifier/au/lender_document_report
   debt_qualifier/gb/broker_fee
   debt_qualifier/gb/browse_product
   debt_qualifier/gb/credit_history_questions
   debt_qualifier/gb/custom_panel
   debt_qualifier/gb/kfi_complaint_setting
   debt_qualifier/gb/panel_provider
   ;; digital_signature
   digital_signature
   digital_signature/content
   digital_signature/content_path
   digital_signature/notify
   digital_signature/signatories
   digital_signature/signatories/generate_code
   digital_signature/signatories/sign
   ;; disclaimer
   disclaimer/orders
   ;; docnote
   docnote
   docnote-v2
   docnote/attachment
   docnote-v2/attachment
   docnote/attachment-v2
   docnote-v2/attachment-v2
   docnote/attachment-v3
   docnote-v2/attachment-v3
   docnote/attachment-v3/content
   docnote-v2/attachment-v3/content
   docnote-v2/body
   docnote-v2/case
   docnote/category
   docnote-v2/category
   docnote-v2/email
   docnote-v2/firstread
   ;; eapplications
   eapplications/product
   eapplications/product_type
   eapplications/vendor
   eapplications/vendor/product
   eapplications/vendor/product/option
   eapplications/vendor/service
   ;; entity
   entity/client
   entity/client-v2
   entity/client-v3
   entity/client/portfolio
   entity/client-v2/portfolio
   entity/client-v3/portfolio
   entity/client/cashflow
   entity/client-v2/cashflow
   entity/client-v3/cashflow
   entity/user
   entity/user-v2
   ;; portfolio
   portfolio/position
   portfolio/profit_analysis/detail
   portfolio/transaction
   portfolio/transaction_types
   ;; session
   session/capability
   session/password
   session/transaction
   ;; ufield
   ufield
   ))

(defpackage :cl-xplan-api
  (:use :cl :cl-xplan-api/core :cl-xplan-api/api)
  (:export
   ;; export useful items from xplan-api
   xplan-session
   username
   password
   base-url
   api-key
   with-xplan-session
   with-bulk-request
   get-request-by-name
   process-request
   delete-session
   convert-xplan-type-to-native
   convert-native-to-xplan-type
   convert-bulk-to-native
   response
   response-time
   ;; export API
   ;; access
   access/client
   ;; asset_class
   asset_class
   ;; assumption_set
   assumption_set
   assumption_set/risk_profile
   assumption_set/risk_profile/asset_class
   ;; case_manager
   case_manager
   case_manager-v2
   case_manager/benchmark
   case_manager/debt_qualifier_scenario
   case_manager/diary
   case_manager/docnote
   case_manager/fee_for_service
   case_manager/fsg
   case_manager/goal
   case_manager/lead
   case_manager/objective
   case_manager/risk_researcher_scenario
   case_manager/savings
   case_manager/supersolver_scenario
   case_manager/task
   case_manager/thread
   case_manager/xtool_scenario
   ;; class_smsf
   class_smsf/account
   class_smsf/fund
   class_smsf/fund/contribution_caps
   class_smsf/fund/members
   class_smsf/fund_link
   ;; client_message
   client_message
   ;; currency
   currency
   currency-v2
   ;; debt_qualifier
   debt_qualifier/au/lender_document
   debt_qualifier/au/lender_document_report
   debt_qualifier/gb/broker_fee
   debt_qualifier/gb/browse_product
   debt_qualifier/gb/credit_history_questions
   debt_qualifier/gb/custom_panel
   debt_qualifier/gb/kfi_complaint_setting
   debt_qualifier/gb/panel_provider
   ;; digital_signature
   digital_signature
   digital_signature/content
   digital_signature/content_path
   digital_signature/notify
   digital_signature/signatories
   digital_signature/signatories/generate_code
   digital_signature/signatories/sign
   ;; disclaimer
   disclaimer/orders
   ;; docnote
   docnote
   docnote-v2
   docnote/attachment
   docnote-v2/attachment
   docnote/attachment-v2
   docnote-v2/attachment-v2
   docnote/attachment-v3
   docnote-v2/attachment-v3
   docnote/attachment-v3/content
   docnote-v2/attachment-v3/content
   docnote-v2/body
   docnote-v2/case
   docnote/category
   docnote-v2/category
   docnote-v2/email
   docnote-v2/firstread
   ;; eapplications
   eapplications/product
   eapplications/product_type
   eapplications/vendor
   eapplications/vendor/product
   eapplications/vendor/product/option
   eapplications/vendor/service
   ;; entity
   entity/client
   entity/client-v2
   entity/client-v3
   entity/client/portfolio
   entity/client-v2/portfolio
   entity/client-v3/portfolio
   entity/client/cashflow
   entity/client-v2/cashflow
   entity/client-v3/cashflow
   entity/user
   entity/user-v2
   ;; portfolio
   portfolio/position
   portfolio/profit_analysis/detail
   portfolio/transaction
   portfolio/transaction_types
   ;; session
   session/capability
   session/password
   session/transaction
   ;; ufield
   ufield
   ))
