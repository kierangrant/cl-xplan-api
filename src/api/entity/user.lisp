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

File: src/api/entity/user.lisp
Description: /entity/user API Functions
|#

(in-package :cl-xplan-api/api)

;; entity/user - GET /resourceful/entity/user and GET /resourceful/entity/user/:entity_id
(define-entrypoint entity/user :get
  (entity_id)
  (fields
   ((images_as_base64 nil base64-p) :cond base64-p :value (if images_as_base64 1 0))
   (ids :cond (and (not entity_id) ids))
   (page :cond (and (not entity_id) page))
   (quicksearch :cond (and (not entity_id) quicksearch))
   ((title_fields nil fields-p) :cond (and (not entity_id) fields-p) :value (if title_fields 1 0))
   ((include_prospect nil prospect-p) :cond (and (not entity_id) prospect-p) :value (if include_prospect 1 0)))
  :resource (format NIL "/entity/user~@[/~A~]" entity_id))

;; entity/user - PATCH /resourceful/entity/user/:entity_id
(define-entrypoint entity/user :patch
  (entity_id) (fields)
  :single-parms-as-body T
  :resource (format NIL "/entity/user/~A" entity_id))

;; entity/user-v2 - GET /resourceful/entity/user-v2 and GET /resourceful/entity/user-v2/:entity_id
(define-entrypoint entity/user-v2 :get
  (entity_id)
  (fields
   ((images_as_base64 nil base64-p) :cond base64-p :value (if images_as_base64 1 0))
   (ids :cond (and (not entity_id) ids))
   (quicksearch :cond (and (not entity_id) quicksearch))
   (group_ids :cond (and (not entity_id) group_ids))
   (group_name :cond (and (not entity_id) group_name))
   (page_size :cond (and (not entity_id) page_size))
   (page_sort :cond (and (not entity_id) page_sort))
   (page_bookmark :cond (and (not entity_id) page_bookmark))
   (page_dir :cond (and (not entity_id) page_dir))
   ((title_fields nil title-p) :cond (and (not entity_id) title-p) :value (if title_fields 1 0)))
  :documentation "Retrieve a collection of users.
Use ids for multiple Entities (entity/user-v2) else use entity_id (entity/user-v2/:entity_id)"
  :resource (format nil "/entity/user-v2~@[/~A~]" entity_id))

;; entity/user-v2 - POST /resourceful/entity/user-v2
(define-entrypoint entity/user-v2 :post
  () (access-level billing_group fields user_id password)
  :single-parms-as-body T
  :resource "/entity/user-v2")

;; entity/user-v2 - PATCH /resourceful/entity/user-v2/:entity_id
(define-entrypoint entity/user-v2 :patch
  (entity_id) (fields)
  :single-parms-as-body T
  :resource (format nil "/entity/user-v2/~A" entity_id))
