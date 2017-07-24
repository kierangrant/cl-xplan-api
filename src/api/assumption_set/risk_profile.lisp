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

File: src/api/assumption_set/risk_profile.lisp
Description: /assumption_set/risk_profile API Functions
|#

(in-package :cl-xplan-api/api)

;; assumption_set/risk_profile - GET /resourceful/assumption_set/:assumption_set_name/risk_profile and GET /resourceful/assumption_set/:assumption_set_name/risk_profile/:risk_profile

(define-entrypoint assumption_set/risk_profile :get
  (assumption_set_name risk_profile) ()
  :resource (format NIL "/assumption_set/~A/risk_profile~@[/~A~]" assumption_set_name risk_profile))
