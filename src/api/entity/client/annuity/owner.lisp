#|
This file is part of CL-XPLAN-API, the Lisp XPLAN API Library

Copyright (C) 2018 Kieran Grant
This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser General Public License
(http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Lisp Lesser GNU General Public License for more details.

File: src/api/entity/client/annuity/owner.lisp
Description: /entity/client/annuity/owner API functions
|#

(in-package :cl-xplan-api/api)

;; entity/client/annuity/owner - GET /resourceful/entity/client/:entity_id/annuity/:list_obj_index/owner
(define-entrypoint entity/client/annuity/owner :get
  (entity_id list_obj_index) ()
  :resource (format nil "/entity/client/~A/annuity/~A/owner" entity_id list_obj_index))

;; entity/client/annuity/owner - PUT /resourceful/entity/client/:entity_id/annuity/:list_obj_index/owner
(define-entrypoint entity/client/annuity/owner :put
  (entity_id list_obj_index) (percentage owner_id)
  :resource (format nil "/entity/client/~A/annuity/~A/owner" entity_id list_obj_index))

;; entity/client-v2/annuity/owner - GET /resourceful/entity/client-v2/:entity_id/annuity/:list_obj_index/owner
(define-entrypoint entity/client-v2/annuity/owner :get
  (entity_id list_obj_index) ()
  :resource (format nil "/entity/client-v2/~A/annuity/~A/owner" entity_id list_obj_index))

;; entity/client-v2/annuity/owner - PUT /resourceful/entity/client-v2/:entity_id/annuity/:list_obj_index/owner
(define-entrypoint entity/client-v2/annuity/owner :put
  (entity_id list_obj_index) (percentage owner_id)
  :resource (format nil "/entity/client-v2/~A/annuity/~A/owner" entity_id list_obj_index))

;; entity/client-v3/annuity/owner - GET /resourceful/entity/client-v3/:entity_id/annuity/:list_obj_index/owner
(define-entrypoint entity/client-v3/annuity/owner :get
  (entity_id list_obj_index) ()
  :resource (format nil "/entity/client-v3/~A/annuity/~A/owner" entity_id list_obj_index))

;; entity/client-v3/annuity/owner - PUT /resourceful/entity/client-v3/:entity_id/annuity/:list_obj_index/owner
(define-entrypoint entity/client-v3/annuity/owner :put
  (entity_id list_obj_index) (percentage owner_id)
  :resource (format nil "/entity/client-v3/~A/annuity/~A/owner" entity_id list_obj_index))
