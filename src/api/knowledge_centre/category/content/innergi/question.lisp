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

File: src/api/knowledge_centre/category/content/innergi/question.lisp
Description: /knowledge_centre/category/content/innergi/question API functions
|#

(in-package :cl-xplan-api/api)

;; knowledge_centre/category/content/innergi/question - GET /resourceful/knowledge_centre/category/:category_id/content/innergi/:content_id/question and GET /resourceful/knowledge_centre/category/:category_id/content/innergi/:content_id/question/:question_id
(define-entrypoint knowledge_centre/category/content/innergi/question :get
  (category_id content_id question_id) (access_token)
  :resource (format nil "/knowledge_centre/category/~A/content/innergi/~A/question~@[/~A~]"
		    category_id content_id question_id))
