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

File: src/api/commission/supplier.lisp
Description: /commission/supplier API functions
|#

(in-package :cl-xplan-api/api)

;; commission/supplier - GET /resourceful/commission/supplier
(define-entrypoint commission/supplier :get
  ()
  (category_id
   current_supplier_id
   (X-XPLAN_AUTHENTICATION :string "X-XPLAN_AUTHENTICATION"
			   :documentation "userId=[user id:int]&siteCode=[siteCode:string]"))
  :resource "/commission/supplier"
  :documentation "Standalone handler for supplier")
