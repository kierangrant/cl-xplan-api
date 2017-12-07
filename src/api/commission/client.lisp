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

File: src/api/commission/client.lisp
Description: /commission/client API functions
|#

(in-package :cl-xplan-api/api)

;; commission/client - POST /resourceful/commission/client

(define-entrypoint commission/client :post
  ()
  ((first_name :documentation "The client's first name")
   (last_name :documentation "The client's surname")
   (display_name :documentation "The client's display name (read only)")
   client_external_id
   (referrer_original_id :documentation "The XPLAN original id for the referrer")
   (related_original_id
    :documentation "The original ID of the client/partner depending on who was are in the relationship")
   ((is_business nil is_business-p) :cond is_business-p :value (if is_business 1 0) :documentation "IsBusiness")
   (external_system_id :documentation "ExternalSystemID")
   (client_original_id :documentation "The XPLAN original id for the client")
   (preferred_name :documentation "The client's preferred name")
   (anniversary_date :documentation "AnniversaryDate")
   (company_name :documentation "The client's company name")
   (adviser_original_id :documentation "The XPLAN original id for the adviser")
   ((is_partner nil is_partner-p) :cond is_partner-p :value (if is_partner 1 0) :documentation "IsPartner")
   (contact_name :documentation "The client's contact name")
   (client_created_date :documentation "ClientCreateDate --> XplanCreateDate on record"))
  :documentation "Collection handler for client"
  :resource "/commission/client")

;; commission/client - GET /resourceful/commission/client/:original_id

(define-entrypoint commission/client :get
  (original_id)
  ((X-XPLAN_AUTHENTICATION :string "X-XPLAN_AUTHENTICATION"
			   :documentation "userId=[user id:int]&siteCode=[siteCode:string]"))
  :resource (format nil "/commission/client/~A" original_id)
  :documentation "Item handler for client")
