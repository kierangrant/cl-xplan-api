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

File: src/api/entity/client/@dynamic.lisp
Description: /entity/client/<dynamic> API functions
|#

(in-package :cl-xplan-api/api)

;;; This is the dynamic handler for entity list items that don't have their own handler

;; entity/client/<dynamic> - GET /resourceful/entity/client/:entity_id/:list_name and  /resourceful/entity/client/:entity_id/:list_name/:list_obj_index
(define-entrypoint entity/client/<dynamic> :get
  (entity_id list_name list_obj_index)
  ((indexes :cond (and (not list_obj_index) indexes))
   fields
   (page :cond (and (not list_obj_index) page)))
  :resource (format nil "/entity/client/~A/~A~@[/~A~]" entity_id list_name list_obj_index))

;; entity/client/<dynamic> - POST /resourceful/entity/client/:entity_id/:list_name
(define-entrypoint entity/client/<dynamic> :post
  (entity_id list_name) (fields extra_return_fields)
  :resource (format nil "/entity/client/~A/~A" entity_id list_name))

;; entity/client/<dynamic> - PATCH /resourceful/entity/client/:entity_id/:list_name/:list_obj_index
(define-entrypoint entity/client/<dynamic> :patch
  (entity_id list_name list_obj_index) (fields extra_return_fields)
  :resource (format nil "/entity/client/~A/~A/~A" entity_id list_name list_obj_index))

;; entity/client/<dynamic> - DELETE /resourceful/entity/client/:entity_id/:list_name/:list_obj_index
(define-entrypoint entity/client/<dynamic> :delete
  (entity_id list_name list_obj_index) ()
  :resource (format nil "/entity/client/~A/~A/~A" entity_id list_name list_obj_index))

;; entity/client-v2/<dynamic> - GET /resourceful/entity/client-v2/:entity_id/:list_name and GET /resourceful/entity/client-v2/:entity_id/:list_name/:list_obj_index
(define-entrypoint entity/client-v2/<dynamic> :get
  (entity_id list_name list_obj_index)
  ((indexes :cond (and (not list_obj_index) indexes))
   fields
   (page :cond (and (not list_obj_index) page)))
  :resource (format nil "/entity/client-v2/~A/~A~@[/~A~]" entity_id list_name list_obj_index))

;; entity/client-v2/<dynamic> - POST /resourceful/entity/client-v2/:entity_id/:list_name
(define-entrypoint entity/client-v2/<dynamic> :post
  (entity_id list_name) (fields extra_return_fields)
  :resource (format nil "/entity/client-v2/~A/~A" entity_id list_name))

;; entity/client-v2/<dynamic> - PATCH /resourceful/entity/client-v2/:entity_id/:list_name/:list_obj_index
(define-entrypoint entity/client-v2/<dynamic> :patch
  (entity_id list_name list_obj_index) (fields extra_return_fields)
  :resource (format nil "/entity/client-v2/~A/~A/~A" entity_id list_name list_obj_index))

;; entity/client-v2/<dynamic> - DELETE /resourceful/entity/client-v2/:entity_id/:list_name/:list_obj_index
(define-entrypoint entity/client-v2/<dynamic> :delete
  (entity_id list_name list_obj_index) ()
  :resource (format nil "/entity/client-v2/~A/~A/~A" entity_id list_name list_obj_index))

;; entity/client-v3/<dynamic> - GET /resourceful/entity/client-v3/:entity_id/:list_name and GET /resourceful/entity/client-v3/:entity_id/:list_name/:list_obj_index
(define-entrypoint entity/client-v3/<dynamic> :get
  (entity_id list_name list_obj_index)
  ((indexes :cond (and (not list_obj_index) indexes))
   fields
   (page :cond (and (not list_obj_index) page)))
  :resource (format nil "/entity/client-v3/~A/~A~@[/~A~]" entity_id list_name list_obj_index))

;; entity/client-v3/<dynamic> - POST /resourceful/entity/client-v3/:entity_id/:list_name
(define-entrypoint entity/client-v3/<dynamic> :post
  (entity_id list_name) (fields extra_return_fields)
  :resource (format nil "/entity/client-v3/~A/~A" entity_id list_name))

;; entity/client-v3/<dynamic> - PATCH /resourceful/entity/client-v3/:entity_id/:list_name/:list_obj_index
(define-entrypoint entity/client-v3/<dynamic> :patch
  (entity_id list_name list_obj_index) (fields extra_return_fields)
  :resource (format nil "/entity/client-v3/~A/~A/~A" entity_id list_name list_obj_index))

;; entity/client-v3/<dynamic> - DELETE /resourceful/entity/client-v3/:entity_id/:list_name/:list_obj_index
(define-entrypoint entity/client-v3/<dynamic> :delete
  (entity_id list_name list_obj_index) ()
  :resource (format nil "/entity/client-v3/~A/~A/~A" entity_id list_name list_obj_index))
