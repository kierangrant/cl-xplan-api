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

File: src/api/other_allocation.lisp
Description: /other_allocation API functions
|#

(in-package :cl-xplan-api/api)

;; other_allocation - GET /resourceful/other_allocation and GET /resourceful/other_allocation/:other_id
(define-entrypoint other_allocation :get
  (other_id) ()
  :resource (format nil "/other_allocation~@[/~A~]" other_id))
