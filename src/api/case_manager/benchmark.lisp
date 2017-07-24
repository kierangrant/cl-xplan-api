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

File: src/api/case_manager/benchmark.lisp
Description: /case_manager/benchmark API Functions
|#

(in-package :cl-xplan-api/api)

;; case_manager/benchmark - GET /resourceful/case_manager/:container_id/benchmark
(define-entrypoint case_manager/benchmark :get (container_id) ()
		   :resource (format NIL "/case_manager/~A/benchmark" container_id))
