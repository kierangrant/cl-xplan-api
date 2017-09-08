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

File: src/api/family_tree/relation.lisp
Description: /family_tree/relation API functions
|#

(in-package :cl-xplan-api/api)

;; family_tree/relation - GET /resourceful/family_tree/:family_tree_id/relation and GET /resourceful/family_tree/:family_tree_id/relation/:relation_id
(define-entrypoint family_tree/relation :get
  (family_tree_id relation_id) ()
  :resource (format nil "/family_tree/~A/relation~@[/~A~]" family_tree_id relation_id))

;; family_tree/relation - POST /resourceful/family_tree/:family_tree_id/relation
(define-entrypoint family_tree/relation :post
  (family_tree_id) (parent_id child_id relation_type)
  :resource (format nil "/family_tree/~A/relation" family_tree_id))

;; family_tree/relation - PATCH /resourceful/family_tree/:family_tree_id/relation/:relation_id
(define-entrypoint family_tree/relation :patch
  (family_tree_id relation_id) (relation_type id)
  :resource (format nil "/family_tree/~A/relation/~A" family_tree_id relation_id))

;; family_tree/relation - DELETE /resourceful/family_tree/:family_tree_id/relation/:relation_id
(define-entrypoint family_tree/relation :delete
  (family_tree_id relation_id) () :resource (format nil "/family_tree/~A/relation/~A" family_tree_id relation_id))
