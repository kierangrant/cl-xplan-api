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

File: src/api/session/user.lisp
Description: /session/user API functions
|#

(in-package :cl-xplan-api/api)

;; session/user - GET /resourceful/session/user
(define-entrypoint session/user :get
  () ()
  :documentation "Return information about the user for the current session."
  :resource "/session/user")
