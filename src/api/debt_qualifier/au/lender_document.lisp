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

File: src/api/debt_qualifier/au/lender_document.lisp
Description: /debt_qualifier/au/lender_document API Functions
|#

(in-package :cl-xplan-api/api)

;; debt_qualifier/au/lender_document - GET /resourceful/debt_qualifier/au/lender_document/:lender_id
(define-entrypoint debt_qualifier/au/lender_document :get
  (lender_id) () :documentation "Get vector of lender documents for a lender"
  :resource (format nil "/debt_qualifier/au/lender_document/~A" lender_id))
