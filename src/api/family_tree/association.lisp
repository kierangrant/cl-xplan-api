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

File: src/api/family_tree/association.lisp
Description: /family_tree/association API functions
|#

(in-package :cl-xplan-api/api)

;; family_tree/association - GET /resourceful/family_tree/:family_tree_id/association
(define-entrypoint family_tree/association :get
  (family_tree_id) ()
  :resource (format nil "/family_tree/~A/association" family_tree_id))

;; family_tree/association - POST /resourceful/family_tree/:family_tree_id/association
(define-entrypoint family_tree/association :post
  (family_tree_id) (participants type)
  :documentation "Note, participants is an array of objects (hash-table, a-list, p-list), not an array of integers"
  :resource (format nil "/family_tree/~A/association" family_tree_id))

;; family_tree/association - PUT /resourceful/family_tree/:family_tree_id/association/:association_id
(define-entrypoint family_tree/association :put
  (family_tree_id association_id)
  (participants type id)
  :documentation "Note, participants is an array of objects (hash-table, a-list, p-list), not an array of integers"
  :resource (format nil "/family_tree/~A/association/~A" family_tree_id association_id))
