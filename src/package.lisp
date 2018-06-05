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
   #:xplan-request #:xplan-request-bulk #:xplan-bulk-requests #:xplan-session
   #:xplan-iso8601 #:xplan-date #:xplan-time #:xplan-currency #:xplan-binary
   ;; conditions
   #:xplan-api-error #:xplan-api-error-bad-request #:xplan-api-error-unauthenticated #:xplan-api-error-unauthorised
   #:xplan-api-error-not-found #:xplan-api-error-server-error #:xplan-api-error-upstream-server-error
   #:xplan-api-error-code #:xplan-api-error-reason #:xplan-api-error-request
   ;; methods
   #:api-key #:username #:password #:base-url #:transport-version #:xplan-session-auto-reauth #:force-init-auth
   #:response #:response-msg #:response-code #:response-headers #:resource #:request-method #:parameters #:content
   #:content-type #:name #:omit-results-on-success #:response-time #:requests #:include-subtimings
   #:xplan-iso8601-str #:xplan-currency-code #:xplan-value
   
   #:delete-session #:prepare-request #:process-request #:user-agent #:xplan-api-call
   #:xplan-api-error-status-code #:xplan-api-error-reason-message
   #:xplan-api-error-request #:xplan-session-auto-reauth #:get-request-by-name
   ;; functions
   #:convert-xplan-type-to-native #:convert-native-to-xplan-type #:convert-bulk-to-native
   ;; macros
   #:with-xplan-session #:with-bulk-request
   ;; variables
   #:*max-rounding* #:*xplan-api-debug*))

;; Deleting existing packages
(if (find-package :cl-xplan-api)
    (delete-package (find-package :cl-xplan-api)))

(if (find-package :cl-xplan-api/api)
    (delete-package (find-package :cl-xplan-api/api)))

;; Rrecreate cl-xplan-api/api package. Exported symbols will be done by the macros that define the APIs
(make-package :cl-xplan-api/api :use '(:cl)) ; not going to directly use cl-xplan-api/core
(import '(cl-xplan-api/core::define-entrypoint cl-xplan-api/core::cond-hash
	  cl-xplan-api/core::define-dynamiclike-entrypoints)
	(find-package :cl-xplan-api/api))

;; recreate the cl-xplan-api pacakge. Re-export what we need from cl-xplan-api/core, API symbols will be exported
;; by the macros directly
(make-package :cl-xplan-api :use '(:cl :cl-xplan-api/core :cl-xplan-api/api))
;; These symbols are to be re-exported from cl-xplan-api/core
(export '(cl-xplan-api/core:xplan-session cl-xplan-api/core:username cl-xplan-api/core:password
	  cl-xplan-api/core:base-url cl-xplan-api/core:api-key cl-xplan-api/core:with-xplan-session
	  cl-xplan-api/core:with-bulk-request cl-xplan-api/core:get-request-by-name
	  cl-xplan-api/core:process-request cl-xplan-api/core:delete-session
	  cl-xplan-api/core:convert-xplan-type-to-native cl-xplan-api/core:convert-native-to-xplan-type
	  cl-xplan-api/core:convert-bulk-to-native cl-xplan-api/core:response cl-xplan-api/core:response-time)
	(find-package :cl-xplan-api))
