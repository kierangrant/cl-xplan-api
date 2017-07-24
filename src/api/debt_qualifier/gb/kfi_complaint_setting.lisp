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

File: src/api/debt_qualifier/gb/kfi_complaint_setting.lisp
Description: /debt_qualifier/gb/kfi_complaint_setting API Functions
|#

(in-package :cl-xplan-api/api)

;; debt_qualifier/gb/kfi_complaint_setting - GET /resourceful/debt_qualifier/gb/kfi_complaint_setting/:group_id
(define-entrypoint debt_qualifier/gb/kfi_complaint_setting :get (group_id) ()
		   :resource (format nil "/debt_qualifier/gb/kfi_complaint_setting/~A" group_id))

;; debt_qualifier/gb/kfi_complaint_setting - PATCH /resourceful/debt_qualifier/gb/kfi_complaint_setting/:group_id
(define-entrypoint debt_qualifier/gb/kfi_complaint_setting :patch (group_id)
		   (block street_name suburb state country postcode department contact url email
			  country_code phone_number)
		   :resource (format nil "/debt_qualifier/gb/kfi_complaint_setting/~A" group_id))
