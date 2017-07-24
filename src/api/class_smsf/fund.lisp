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

File: src/api/class_smsf/fund.lisp
Description: /class_smsf/fund API Functions
|#

(in-package :cl-xplan-api/api)

;; class_smsf/fund - GET /resourceful/class_smsf/fund and GET /resourceful/class_smsf/fund/:fund
(define-entrypoint class_smsf/fund :get (fund) ()
		   :resource (format nil "/class_smsf/fund~@[/~A~]" fund)
		   :documentation "If fund isn't provided is 'Collection handler under portfolio /class_smsf/fund' otherwise it is 'Link handler under class_smsf /class_smsf/fund/<fund_code>'")
