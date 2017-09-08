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

File: src/api/session/user/app_storage.lisp
Description: /session/user/app_storage API functions
|#

(in-package :cl-xplan-api/api)

;; session/user/app_storage - GET /resourceful/session/user/app_storage
(define-entrypoint session/user/app_storage :get
  () (keys)
  :documentation "Return app storage for the current session."
  :resource "/session/user/app_storage")

;; session/user/app_storage - POST /resourceful/session/user/app_storage
(define-entrypoint session/user/app_storage :post
  () (key value)
  :documentation "Create/Update the app storage for the current session."
  :resource "/session/user/app_storage")
