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

File: src/api/entity.lisp
Description: /entity API Functions
|#

(in-package :cl-xplan-api/api)

;;; entity

;; entity/client - GET /resourceful/entity/client and GET /resourceful/entity/client/:entity_id
(cl-xplan-api/core::define-entrypoint entity/client :get
  (entity_id)
  (fields
   (ids :cond (and (not entity_id) ids))
   (page :cond (and (not entity_id) page))
   (quicksearch :cond (and (not entity_id) quicksearch))
   ((images_as_base64 nil base64-p) :cond base64-p :value (if images_as_base64 1 0)) 
   ((title_fields nil title-p) :cond (and (not entity_id) title-p) :value (if title_fields 1 0))
   ((include_prospect nil prospect-p) :cond (and (not entity_id) prospect-p) :value (if include_prospect 1 0)))
  :resource (format nil "/entity/client~@[/~A~]" entity_id))

;; entity/client - POST /resourceful/entity/client
(cl-xplan-api/core::define-entrypoint entity/client :post () (fields) :resource "/entity/client")

;; entity/client - PATCH /resourceful/entity/client/:entity_id
(cl-xplan-api/core::define-entrypoint entity/client :patch (entity_id) (fields) :resource (format NIL "/entity/client/~A" entity_id))

;; entity/client-v2 - GET /resourceful/entity/client-v2 and GET /resourceful/entity/client-v2/:entity_id
(cl-xplan-api/core::define-entrypoint entity/client-v2 :get
  (entity_id)
  (fields
   ((images_as_base64 nil base64-p) :cond base64-p :value (if images_as_base64 1 0))
   (ids :cond (and (not entity_id) ids))
   (page :cond (and (not entity_id) page))
   (quicksearch :cond (and (not entity_id) quicksearch))
   (include_client_status :cond (and (not entity_id) include_client_status))
   ((title_fields nil title-p) :cond (and (not entity_id) title-p) :value (if title_fields 1 0))
   ((include_prospect nil prospect-p) :cond (and (not entity_id) prospect-p) :value (if include_prospect 1 0)))
  :resource (format nil "/entity/client-v2~@[/~A~]" entity_id))

;; entity/client-v2 - POST /resourceful/entity/client-v2
(cl-xplan-api/core::define-entrypoint entity/client-v2 :post () (fields) :resource "/entity/client-v2")

;; entity/client-v2 - PATCH /resourceful/entity/client-v2/:entity_id
(cl-xplan-api/core::define-entrypoint entity/client-v3 :patch (entity_id) (fields) :resource (format NIL "/entity/client-v2/~A" entity_id))

;; entity/client-v3 - GET /resourceful/entity/client-v3 and GET /resourceful/entity/client-v3/:entity_id
(cl-xplan-api/core::define-entrypoint
    entity/client-v3
    :get
  (entity_id)
  (fields ((images_as_base64 nil base64-p) :cond base64-p :value (if images_as_base64 1 0))
	  (ids :cond (and (not entity_id) ids))
	  (quicksearch :cond (and (not entity_id) quicksearch))
	  (include_client_status :cond (and (not entity_id) include_client_status))
	  (include_client_category :cond (and (not entity_id) include_client_category))
	  (adviser_id :cond (and (not entity_id) adviser_id))
	  (entity_type :cond (and (not entity_id) entity_type))
	  (profadviser_id :cond (and (not entity_id) profadviser_id))
	  (profadviser_name :cond (and (not entity_id) profadviser_name))
	  (group_ids :cond (and (not entity_id) group_ids))
	  (group_name :cond (and (not entity_id) group_name))
	  (page_size :cond (and (not entity_id) page_size))
	  (page_sort :cond (and (not entity_id) page_sort))
	  (page_bookmark :cond (and (not entity_id) page_bookmark))
	  (page_dir :cond (and (not entity_id) page_dir)))
  :documentation "Retrieve a collection of clients.
Use ids for multiple Entities (entity/client-v3) else use entity_id (entity/client-v3/:entity_id)"
  :resource (format nil "/entity/client-v3~@[/~A~]" entity_id))

;; entity/client/portfolio - GET /resourceful/entity/client/:entity_id/portfolio
(cl-xplan-api/core::define-entrypoint entity/client/portfolio :get (entity_id) (fields) :resource (format NIL "/entity/client/~A/portfolio" entity_id))

;; entity/client-v2/portfolio - GET /resourceful/entity/client-v2/:entity_id/portfolio
(cl-xplan-api/core::define-entrypoint entity/client-v2/portfolio :get (entity_id) (fields) :resource (format NIL "/entity/client-v2/~A/portfolio" entity_id))

;; entity/client-v3/portfolio - GET /resourceful/entity/client-v3/:entity_id/portfolio
(cl-xplan-api/core::define-entrypoint entity/client-v3/portfolio :get (entity_id) (fields) :resource (format NIL "/entity/client-v3/~A/portfolio" entity_id))

;; entity/user - GET /resourceful/entity/user and GET /resourceful/entity/user/:entity_id
(cl-xplan-api/core::define-entrypoint entity/user :get
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
(cl-xplan-api/core::define-entrypoint entity/user :patch (entity_id) (fields) :resource (format NIL "/entity/user/~A" entity_id))

;; entity/user-v2 - GET /resourceful/entity/user-v2 and GET /resourceful/entity/user-v2/:entity_id
(cl-xplan-api/core::define-entrypoint entity/user-v2 :get
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
(cl-xplan-api/core::define-entrypoint entity/user-v2 :post () (access-level billing_group fields user_id password) :resource "/entity/user-v2")

;; entity/user-v2 - PATCH /resourceful/entity/user-v2/:entity_id
(cl-xplan-api/core::define-entrypoint entity/user-v2 :patch (entity_id) (fields) :resource (format nil "/entity/user-v2/~A" entity_id))
