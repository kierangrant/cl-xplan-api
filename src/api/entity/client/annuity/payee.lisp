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

File: src/api/entity/client/annuity/payee.lisp
Description: /entity/client/annuity/payee API functions
|#

(in-package :cl-xplan-api/api)

;; entity/client/annuity/payee - GET /resourceful/entity/client/:entity_id/annuity/:list_obj_index/payee
(define-entrypoint entity/client/annuity/payee :get
  (entity_id list_obj_index) ()
  :documentation "List of payees of an annuity."
  :resource (format nil "/entity/client/~A/annuity/~A/payee" entity_id list_obj_index))

;; entity/client/annuity/payee - PUT /resourceful/entity/client/:entity_id/annuity/:list_obj_index/payee
(define-entrypoint entity/client/annuity/payee :put
  (entity_id list_obj_index) (percentage payee_id payee_name)
  :resource (format nil "/entity/client/~A/annuity/~A/payee" entity_id list_obj_index))

;; entity/client-v2/annuity/payee - GET /resourceful/entity/client-v2/:entity_id/annuity/:list_obj_index/payee
(define-entrypoint entity/client-v2/annuity/payee :get
  (entity_id list_obj_index) ()
  :documentation "List of payees of an annuity."
  :resource (format nil "/entity/client-v2/~A/annuity/~A/payee" entity_id list_obj_index))

;; entity/client-v2/annuity/payee - PUT /resourceful/entity/client-v2/:entity_id/annuity/:list_obj_index/payee
(define-entrypoint entity/client-v2/annuity/payee :put
  (entity_id list_obj_index) (percentage payee_id payee_name)
  :resource (format nil "/entity/client-v2/~A/annuity/~A/payee" entity_id list_obj_index))

;; entity/client-v3/annuity/payee - GET /resourceful/entity/client-v3/:entity_id/annuity/:list_obj_index/payee
(define-entrypoint entity/client-v3/annuity/payee :get
  (entity_id list_obj_index) ()
  :documentation "List of payees of an annuity."
  :resource (format nil "/entity/client-v3/~A/annuity/~A/payee" entity_id list_obj_index))

;; entity/client-v3/annuity/payee - PUT /resourceful/entity/client-v3/:entity_id/annuity/:list_obj_index/payee
(define-entrypoint entity/client-v3/annuity/payee :put
  (entity_id list_obj_index) (percentage payee_id payee_name)
  :resource (format nil "/entity/client-v3/~A/annuity/~A/payee" entity_id list_obj_index))
