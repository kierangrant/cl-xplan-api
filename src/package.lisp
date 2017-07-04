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
   password
   prepare-request
   process-request
   parameters
   transport-version
   response
   response-code
   response-msg
   response-headers
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
   ;; variables
   *max-rounding*
   *xplan-api-debug*
   ))

(defpackage :cl-xplan-api/api
  (:use :cl)
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
   ;; entity
   entity/client
   entity/client-v2
   entity/client-v3
   entity/client/portfolio
   entity/client-v2/portfolio
   entity/client-v3/portfolio
   entity/user
   entity/user-v2
   ;; portfolio
   portfolio/position
   portfolio/profit_analysis/detail
   portfolio/transaction
   portfolio/transaction_types
   ;; session
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
   ;; entity
   entity/client
   entity/client-v2
   entity/client-v3
   entity/client/portfolio
   entity/client-v2/portfolio
   entity/client-v3/portfolio
   entity/user
   entity/user-v2
   ;; portfolio
   portfolio/position
   portfolio/profit_analysis/detail
   portfolio/transaction
   portfolio/transaction_types
   ;; session
   session/transaction
   ;; ufield
   ufield
   ))