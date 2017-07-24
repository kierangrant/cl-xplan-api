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

File: src/api/debt_qualifier/gb/panel_provider.lisp
Description: /debt_qualifier/gb/panel_provider API Functions
|#

(in-package :cl-xplan-api/api)

;; debt_qualifier/gb/panel_provider - GET /resourceful/debt_qualifier/gb/panel_provider/:panel_group
(define-entrypoint debt_qualifier/gb/panel_provider :get (panel_group) ()
		   :resource (format nil "/debt_qualifier/gb/panel_provider/~A" panel_group)
		   :documentation "Get list of panel providers and packagers for a panel group from Trigold")
