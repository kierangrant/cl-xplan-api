#| -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

This file is part of CL-XPLAN-API, the Lisp XPLAN API Library
Copyright (C) 2018 Kieran Grant

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

See LICENSE for the full license

File: src/package.lisp
Description: Package definition for CL-XPLAN-API
|#

(in-package :cl-user)

(defpackage :cl-xplan-api
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
   #:with-xplan-session #:with-bulk-request #:xplan-call #:xplan-prepare-bulk
   ;; variables
   #:*max-rounding* #:*xplan-api-debug* #:*xplan-session* #:xplan-batched-request))
