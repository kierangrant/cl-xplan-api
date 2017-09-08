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

File: src/api/educational_content/category.lisp
Description: /educational_content/category API functions
|#

(in-package :cl-xplan-api/api)

;; educational_content/category - GET /resourceful/educational_content/category
(define-entrypoint educational_content/category :get
  () (category_id)
  :resource "/educational_content/category")
