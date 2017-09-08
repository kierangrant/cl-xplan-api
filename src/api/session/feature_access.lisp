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

File: src/api/session/feature_access.lisp
Description: /session/feature_access API functions
|#

(in-package :cl-xplan-api/api)

;; session/feature_access - GET /resourceful/session/feature_access
(define-entrypoint session/feature_access :get
  () (feature_names)
  :resource "/session/feature_access")
