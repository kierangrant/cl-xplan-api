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

File: src/api/entity/client/cashflow.lisp
Description: /entity/client/cashflow API Functions
|#

(in-package :cl-xplan-api/api)

;;; entity/client/cashflow

;; entity/client/cashflow - GET /resourceful/entity/client/:entity_id/cashflow and GET /resourceful/entity/client/:entity_id/cashflow/:list_obj_index
(define-entrypoint entity/client/cashflow :get
  (entity_id list_obj_index)
  ((indexes :cond (and (not list_obj_index) indexes))
   fields
   (page :cond (and (not list_obj_index) page)))
  :resource (format nil "/entity/client/~A/cashflow~@[/~A~]" entity_id list_obj_index))

;; entity/client/cashflow - POST /resourceful/entity/client/:entity_id/cashflow
(define-entrypoint entity/client/cashflow :post
  (entity_id)
  (fields extra_return_fields)
  :single-parms-as-body T
  :resource (format nil "/entity/client/~A/cashflow" entity_id))

;; entity/client/cashflow - PATCH /resourceful/entity/client/:entity_id/cashflow/:list_obj_index
(define-entrypoint entity/client/cashflow :patch
  (entity_id list_obj_id)
  (fields extra_return_fields)
  :single-parms-as-body T
  :resource (format nil "/entity/client/~A/cashflow/~A" entity_id list_obj_id))

;; entity/client/cashflow - DELETE /resourceful/entity/client/:entity_id/cashflow/:list_obj_index
(define-entrypoint entity/client/cashflow :delete
  (entity_id list_obj_id) ()
  :resource (format nil "/entity/client/~A/cashflow/~A" entity_id list_obj_id))

;;; entity/client-v2/cashflow

;; entity/client-v2/cashflow - GET /resourceful/entity/client-v2/:entity_id/cashflow and GET /resourceful/entity/client-v2/:entity_id/cashflow/:list_obj_index
(define-entrypoint entity/client-v2/cashflow :get
  (entity_id list_obj_index)
  ((indexes :cond (and (not list_obj_index) indexes))
   fields
   (page :cond (and (not list_obj_index) page)))
  :resource (format nil "/entity/client-v2/~A/cashflow~@[/~A~]" entity_id list_obj_index))

;; entity/client-v2/cashflow - POST /resourceful/entity/client-v2/:entity_id/cashflow
(define-entrypoint entity/client-v2/cashflow :post
  (entity_id)
  (fields extra_return_fields)
  :single-parms-as-body t
  :resource (format nil "/entity/client-v2/~A/cashflow" entity_id))

;; entity/client-v2/cashflow - PATCH /resourceful/entity/client-v2/:entity_id/cashflow/:list_obj_index
(define-entrypoint entity/client-v2/cashflow :patch
  (entity_id list_obj_id)
  (fields extra_return_fields)
  :single-parms-as-body t
  :resource (format nil "/entity/client-v2/~A/cashflow/~A" entity_id list_obj_id))

;; entity/client-v2/cashflow - DELETE /resourceful/entity/client-v2/:entity_id/cashflow/:list_obj_index
(define-entrypoint entity/client-v2/cashflow :delete
  (entity_id list_obj_id) ()
  :resource (format nil "/entity/client-v2/~A/cashflow/~A" entity_id list_obj_id))

;;; entity/client-v3/cashflow

;; entity/client-v3/cashflow - GET /resourceful/entity/client-v3/:entity_id/cashflow and GET /resourceful/entity/client-v3/:entity_id/cashflow/:list_obj_index
(define-entrypoint entity/client-v3/cashflow :get
  (entity_id list_obj_index)
  ((indexes :cond (and (not list_obj_index) indexes))
   fields
   (page :cond (and (not list_obj_index) page)))
  :resource (format nil "/entity/client-v3/~A/cashflow~@[/~A~]" entity_id list_obj_index))

;; entity/client-v3/cashflow - POST /resourceful/entity/client-v3/:entity_id/cashflow
(define-entrypoint entity/client-v3/cashflow :post
  (entity_id)
  (fields extra_return_fields)
  :single-parms-as-body t
  :resource (format nil "/entity/client-v3/~A/cashflow" entity_id))

;; entity/client-v3/cashflow - PATCH /resourceful/entity/client-v3/:entity_id/cashflow/:list_obj_index
(define-entrypoint entity/client-v3/cashflow :patch
  (entity_id list_obj_id)
  (fields extra_return_fields)
  :single-parms-as-body t
  :resource (format nil "/entity/client-v3/~A/cashflow/~A" entity_id list_obj_id))

;; entity/client-v3/cashflow - DELETE /resourceful/entity/client-v3/:entity_id/cashflow/:list_obj_index
(define-entrypoint entity/client-v3/cashflow :delete
  (entity_id list_obj_id) ()
  :resource (format nil "/entity/client-v3/~A/cashflow/~A" entity_id list_obj_id))
