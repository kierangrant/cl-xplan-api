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

File: src/api/family_tree/person.lisp
Description: /family_tree/person API functions
|#

(in-package :cl-xplan-api/api)

;; family_tree/person - GET /resourceful/family_tree/:family_tree_id/person and GET /resourceful/family_tree/:family_tree_id/person/:person_id
(define-entrypoint family_tree/person :get
  (family_tree_id person_id) ()
  :resource (format nil "/family_tree/~A/person~@[/~A~]" family_tree_id person_id))

;; family_tree/person - POST /resourceful/family_tree/:family_tree_id/person
(define-entrypoint family_tree/person :post
  (family_tree_id)
  (((is_dependant nil is_dependant-p) :cond is_dependant-p :value (if is_dependant 1 0))
   ((add_to_client_group nil add_to_client_group-p) :cond add_to_client_group-p :value (if add_to_client_group 1 0))
   ((should_add_as_new_client nil should_add_as_new_client-p) :cond should_add_as_new_client-p
    :value (if should_add_as_new_client 1 0))
   first_name last_name middle_name gender dob living ancestry dependant_of should_link_to_existing_entity)
  :resource (format nil "/family_tree/~A/person" family_tree_id))

;; family_tree/person - PATCH /resourceful/family_tree/:family_tree_id/person/:person_id
(define-entrypoint family_tree/person :patch
  (family_tree_id person_id)
  (((is_dependant nil is_dependant-p) :cond is_dependant-p :value (if is_dependant 1 0))
   ((add_to_client_group nil add_to_client_group-p) :cond add_to_client_group-p :value (if add_to_client_group 1 0))
   ((should_add_as_new_client nil should_add_as_new_client-p) :cond should_add_as_new_client-p
    :value (if should_add_as_new_client 1 0))
   first_name last_name middle_name gender dob living ancestry dependant_of should_link_to_existing_entity)
  :resource (format nil "/family_tree/~A/person/~A" family_tree_id person_id))

;; family_tree/person - DELETE /resourceful/family_tree/:family_tree_id/person/:person_id
(define-entrypoint family_tree/person :delete
  (family_tree_id person_id) ()
  :resource (format nil "/family_tree/~A/person/~A" family_tree_id person_id))

;; family_tree/person-v2 - GET /resourceful/family_tree/:family_tree_id/person-v2 and GET /resourceful/family_tree/:family_tree_id/person-v2/:person_id
(define-entrypoint family_tree/person-v2 :get
  (family_tree_id person_id) ()
  :resource (format nil "/family_tree/~A/person-v2~@[/~A~]" family_tree_id person_id))

;; family_tree/person-v2 - POST /resourceful/family_tree/:family_tree_id/person-v2
(define-entrypoint family_tree/person-v2 :post
  (family_tree_id)
  (((add_to_client_group nil add_to_client_group-p) :cond add_to_client_group-p :value (if add_to_client_group 1 0))
   ((should_add_as_new_client nil should_add_as_new_client-p) :cond should_add_as_new_client-p
    :value (if should_add_as_new_client 1 0))
   first_name last_name middle_name gender dob living is_dependant ancestry dependant_of
   should_link_to_existing_entity)
  :resource (format nil "/family_tree/~A/person-v2" family_tree_id))

;; family_tree/person-v2 - PATCH /resourceful/family_tree/:family_tree_id/person-v2/:person_id
(define-entrypoint family_tree/person-v2 :patch
  (family_tree_id person_id)
  (((add_to_client_group nil add_to_client_group-p) :cond add_to_client_group-p :value (if add_to_client_group 1 0))
   ((should_add_as_new_client nil should_add_as_new_client-p) :cond should_add_as_new_client-p
    :value (if should_add_as_new_client 1 0))
   first_name last_name middle_name gender dob living is_dependant ancestry dependant_of
   should_link_to_existing_entity)
  :resource (format nil "/family_tree/~A/person-v2/~A" family_tree_id person_id))

;; family_tree/person-v2 - PUT /resourceful/family_tree/:family_tree_id/person-v2/:person_id
(define-entrypoint family_tree/person-v2 :put
  (family_tree_id person_id)
  (((add_to_client_group nil add_to_client_group-p) :cond add_to_client_group-p :value (if add_to_client_group 1 0))
   ((should_add_as_new_client nil should_add_as_new_client-p) :cond should_add_as_new_client-p
    :value (if should_add_as_new_client 1 0))
   first_name last_name middle_name gender dob living is_dependant ancestry dependant_of
   should_link_to_existing_entity)
  :resource (format nil "/family_tree/~A/person-v2/~A" family_tree_id person_id))

;; family_tree/person-v2 - DELETE /resourceful/family_tree/:family_tree_id/person-v2/:person_id
(define-entrypoint family_tree/person-v2 :delete
  (family_tree_id person_id) ()
  :resource (format nil "/family_tree/~A/person-v2/~A" family_tree_id person_id))
