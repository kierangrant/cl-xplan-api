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

File: src/api/knowledge_centre/category.lisp
Description: /knowledge_centre/category API functions
|#

(in-package :cl-xplan-api/api)

;; knowledge_centre/category - GET /resourceful/knowledge_centre/category and GET /resourceful/knowledge_centre/category/:category_id
(define-entrypoint knowledge_centre/category :get
  (category_id) ((parent_category_id :cond (and parent_category_id (not category_id))))
  :resource (format nil "/knowledge_centre/category~@[/~A~]" category_id))
