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

File: src/api/family_tree/model.lisp
Description: /family_tree/model API functions
|#

(in-package :cl-xplan-api/api)

;; GET /resourceful/family_tree/:family_tree_id/model
(define-entrypoint family_tree/model :get
  (family_tree_id) ()
  :resource (format nil "/family_tree/~A/model" family_tree_id))

;; PATCH /resourceful/family_tree/:family_tree_id/model
(define-entrypoint family_tree/model :patch
  (family_tree_id) (model)
  :resource (format nil "/family_tree/~A/model" family_tree_id))
