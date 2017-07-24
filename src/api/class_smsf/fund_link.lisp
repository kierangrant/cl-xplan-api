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

File: src/api/class_smsf/fund_link.lisp
Description: /class_smsf/fund_link API Functions
|#

(in-package :cl-xplan-api/api)

;; class_smsf/fund_link - GET /resourceful/class_smsf/fund_link and GET /resourceful/class_smsf/fund_link/:portfolioid
(define-entrypoint class_smsf/fund_link :get (portfolioid) ()
		   :documentation "Without portfolioid is 'Collection handler under portfolio /class_smsf/fund_link' otherwise is 'Link handler under class_smsf /class_smsf/fund_link/<portfolioid>'"
		   :resource (format nil "/class_smsf/fund_link~@[/~A~]" portfolioid))

;; class_smsf/fund_link - PUT /resourceful/class_smsf/fund_link/:portfolioid
(define-entrypoint class_smsf/fund_link :put (portfolioid) (fund_code)
		   :resource (format nil "/class_smsf/fund_link/~A" portfolioid))
