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

File: src/api/family_tree/person/linked_entity.lisp
Description: /family_tree/person/linked_entity API functions
|#

(in-package :cl-xplan-api/api)

;; GET /resourceful/family_tree/:family_tree_id/person/:person_id/linked_entity
(define-entrypoint family_tree/person/linked_entity :get
  (family_tree_id person_id) ()
  :documentation "The linked entity id for a given Person."
  :resource (format nil "/family_tree/~A/person/~A/linked_entity" family_tree_id person_id))

;; POST /resourceful/family_tree/:family_tree_id/person/:person_id/linked_entity
(define-entrypoint family_tree/person/linked_entity :post
  (family_tree_id person_id)
  (((create_new_entity nil create_new_entity-p) :cond create_new_entity-p :value (if create_new_entity 1 0))
   ((add_to_client_group nil add_to_client_group-p) :cond add_to_client_group-p :value (if add_to_client_group 1 0))
   linked_entity_id dependant_of)
  :resource (format nil "/family_tree/~A/person/~A/linked_entity" family_tree_id person_id))

;; DELETE /resourceful/family_tree/:family_tree_id/person/:person_id/linked_entity/:linked_entity_id
(define-entrypoint family_tree/person/linked_entity :delete
  (family_tree_id person_id linked_entity_id) ()
  :resource (format nil "/family_tree/~A/person/~A/linked_entity/~A" family_tree_id person_id linked_entity_id)
  :documentation "This handler is solely for deleting people")

;; GET /resourceful/family_tree/:family_tree_id/person-v2/:person_id/linked_entity
(define-entrypoint family_tree/person-v2/linked_entity :get
  (family_tree_id person_id) ()
  :documentation "The linked entity id for a given Person."
  :resource (format nil "/family_tree/~A/person-v2/~A/linked_entity" family_tree_id person_id))

;; POST /resourceful/family_tree/:family_tree_id/person-v2/:person_id/linked_entity
(define-entrypoint family_tree/person-v2/linked_entity :post
  (family_tree_id person_id)
  (((create_new_entity nil create_new_entity-p) :cond create_new_entity-p :value (if create_new_entity 1 0))
   ((add_to_client_group nil add_to_client_group-p) :cond add_to_client_group-p :value (if add_to_client_group 1 0))
   linked_entity_id
   dependant_of)
  :resource (format nil "/family_tree/~A/person-v2/~A/linked_entity" family_tree_id person_id))

;; DELETE /resourceful/family_tree/:family_tree_id/person-v2/:person_id/linked_entity/:linked_entity_id
(define-entrypoint family_tree/person-v2/linked_entity :delete
  (family_tree_id person_id linked_entity_id) ()
  :documentation "This handler is solely for deleting people"
  :resource (format nil "/family_tree/~A/person-v2/~A/linked_entity/~A" family_tree_id person_id linked_entity_id))
