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

File: src/api/access.lisp
Description: /access API functions
|#

(in-package :cl-xplan-api/api)

;;; access

;; access - POST /resourceful/access/client/:entityid

(cl-xplan-api/core::define-entrypoint
    access/client
    :post
  (entityid)
  (userid passwd login_mode expiry
	  ((change_passwd nil passwd-p) :cond passwd-p :value (if change_passwd 1 0)))
  :documentation "Create a client resource. Enables COA for a client."
  :resource (format NIL "/access/client/~D" entityid))
