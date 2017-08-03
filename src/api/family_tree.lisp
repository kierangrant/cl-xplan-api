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

File: src/api/family_tree.lisp
Description: /family_tree API functions
|#

(in-package :cl-xplan-api/api)

;; GET /resourceful/family_tree and GET /resourceful/family_tree/:family_tree_id
(define-entrypoint family_tree :get
  (family_tree_id) ((owner :cond (and (not family_tree_id) owner)))
  :resource (format nil "/family_tree~@[/~A~]" family_tree_id))

;; POST /resourceful/family_tree
(define-entrypoint family_tree :post () (owner) :resource "/family_tree")

;; DELETE /resourceful/family_tree/:family_tree_id
(define-entrypoint family_tree :delete (family_tree_id) () :resource (format nil "/family_tree/~A" family_tree_id))

