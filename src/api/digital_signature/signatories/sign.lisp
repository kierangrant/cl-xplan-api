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

File: src/api/digital_signature/signatories/sign.lisp
Description: /digital_signature/signatories/sign API Functions
|#

(in-package :cl-xplan-api/api)

;; digital_signature/signatories/sign - POST /resourceful/digital_signature/:digital_signature_id/signatories/:signatory_id/sign?_method=post
(define-entrypoint digital_signature/signatories/sign :post
  (digital_signature_id signatory_id) (signing_method secure_data otp)
  :single-resource (format nil "/digital_signature/~A/signatories/~A/sign?_method=post"
			   digital_signature_id signatory_id)
  :single-parms-as-body T
  :bulk-resource (format nil "/digital_signature/~A/signatories/~A/sign"
			 digital_signature_id signatory_id))
