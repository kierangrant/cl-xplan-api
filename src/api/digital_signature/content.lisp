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

File: src/api/digital_signature/content.lisp
Description: /digital_signature/content API Functions
|#

(in-package :cl-xplan-api/api)

;; digital_signature/content - GET /resourceful/digital_signature/:digital_signature_id/content
(define-entrypoint digital_signature/content :get
  (digital_signature_id) ()
  :resource (format nil "/digital_signature/~A/content" digital_signature_id))
