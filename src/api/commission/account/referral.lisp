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

File: src/api/commission/account/referral.lisp
Description: /commission/account/referral API functions
|#

(in-package :cl-xplan-api/api)

;; commission/account/referral - GET /resourceful/commission/account/:account_id/referral
(define-entrypoint commission/account/referral :get
  (account_id) ((X-XPLAN_AUTHENTICATION :string "X-XPLAN_AUTHENTICATION"
					:documentation "userId=[user id:int]&siteCode=[siteCode:string]"))
  :resource (format nil "/commission/account/~A/referral" account_id)
  :documentation "Standalone handler for referral")
