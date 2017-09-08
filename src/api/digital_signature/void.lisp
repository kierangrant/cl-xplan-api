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

File: src/api/digital_signature/void.lisp
Description: /digital_signature/void API functions
|#

(in-package :cl-xplan-api/api)

;; digital_signature/void - POST /resourceful/digital_signature/:digital_signature_id/void?_method=post
(define-entrypoint digital_signature/finalise :post
  (digital_signature_id) ()
  :single-resource (format nil "/digital_signature/~A/void?_method=post" digital_signature_id)
  :bulk-resource (format nil "/digital_signature/~A/void" digital_signature_id))
