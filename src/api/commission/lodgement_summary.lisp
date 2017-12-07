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

File: src/api/commission/lodgement_summary.lisp
Description: /commission/lodgement_summary API functions
|#

(in-package :cl-xplan-api/api)

;; commission/lodgement_summary - GET /resourceful/commission/lodgement_summary
(define-entrypoint commission/lodgement_summary :get
  ()
  (adviser_original_id
   client_original_id
   policy_number
   surname
   supplier
   product
   product_classification
   page_sort
   page_bookmark
   (page_dir :documentation "Should be 'next' or 'prev'. Will default to 'next' if neither of these.")
   page_size
   (X-XPLAN_AUTHENTICATION :string "X-XPLAN_AUTHENTICATION"
			   :documentation "userId=[user id:int]&siteCode=[siteCode:string]"))
  :resource "/commission/lodgement_summary"
  :documentation "Standalone handler for lodgement_summary")
