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

File: src/api/commission/policy/feefacilitation.lisp
Description: /commission/policy/feefacilitation API functions
|#

(in-package :cl-xplan-api/api)

;; commission/policy/feefacilitation - GET /resourceful/commission/policy/:original_id/feefacilitation
(define-entrypoint commission/policy/feefacilitation :get
  (original_id)
  ((X-XPLAN_AUTHENTICATION :string "X-XPLAN_AUTHENTICATION"
			   :documentation "userId=[user id:int]&siteCode=[siteCode:string]"))
  :resource (format nil "/commission/policy/~A/feefacilitation" original_id)
  :documentation "Standalone handler for feefacilitation")

;; commission/policy/feefacilitation - PUT /resourceful/commission/policy/:original_id/feefacilitation
(define-entrypoint commission/policy/feefacilitation :put
  (original_id)
  ((commpay_policy_id :documentation "Is an array of objects, please construct manually")
   (allocation_percentage :documentation "Is an array of objects, please construct manually")
   (policy_original_id :documentation "Is an array of objects, please construct manually"))
  :resource (format nil "/commission/policy/~A/feefacilitation" original_id)
  :documentation "commpay_policy_id, allocation_percentage and policy_original_id are arrays of objects, please construct manually")

;; commission/policy/feefacilitation - DELETE /resourceful/commission/policy/:original_id/feefacilitation
(define-entrypoint commission/policy/feefacilitation :delete
  (original_id)
  ((X-XPLAN_AUTHENTICATION :string "X-XPLAN_AUTHENTICATION"
			   :documentation "userId=[user id:int]&siteCode=[siteCode:string]"))
  :resource (format nil "/commission/policy/~A/feefacilitation" original_id))
