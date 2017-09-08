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

File: src/api/pagination_test.lisp
Description: /pagination_test API functions
|#

(in-package :cl-xplan-api/api)

;; pagination_test - GET /resourceful/pagination_test
(define-entrypoint pagination_test :get
  () (extra page_size page_sort page_bookmark page_dir)
  :resource "/pagination_test")
