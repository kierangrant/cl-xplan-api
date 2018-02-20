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

File: src/api/entity/client/@dynamic/attachment.lisp
Description: /entity/client/<dynamic>/attachment API functions
|#

(in-package :cl-xplan-api/api)

;; entity/client/<dynamic>/attachment - GET /resourceful/entity/client/:entity_id/:list_name/:list_obj_index/attachment/:field and GET /resourceful/entity/client/:entity_id/:list_name/:list_obj_index/attachment/:field/:attach_id
(define-entrypoint entity/client/<dynamic>/attachment :get
  (entity_id list_name list_obj_index field attach_id) ()
  :resource (format nil "/entity/client/~A/~A/~A/attachment/~A~@[/~A~]"
		    entity_id list_name list_obj_index field attach_id)
  :documentation "With attach_id:
Display metadata for an attachment entry saved in the provided list object attachment field.
Otherwise:
List the attachments saved in the provided list object attachment field.")

;; entity/client/<dynamic>/attachment - POST /resourceful/entity/client/:entity_id/:list_name/:list_obj_index/attachment/:field
(define-entrypoint entity/client/<dynamic>/attachment :post
  (entity_id list_name list_obj_index field) (filename data)
  :resource (format nil "/entity/client/~A/~A/~A/attachment/~A" entity_id list_name list_obj_index field)
  :documentation "Upload a new attachment to a list object attachment field.")

;; entity/client/<dynamic>/attachment - DELETE /resourceful/entity/client/:entity_id/:list_name/:list_obj_index/attachment/:field/:attach_id
(define-entrypoint entity/client/<dynamic>/attachment :delete
  (entity_id list_name list_obj_index field attach_id) ()
  :resource (format nil "/entity/client/~A/~A/~A/attachment/~A/~A"
		    entity_id list_name list_obj_index field attach_id)
  :documentation "Remove an attachment from the list object attachment field.")

;; entity/client-v2/<dynamic>/attachment - GET /resourceful/entity/client-v2/:entity_id/:list_name/:list_obj_index/attachment/:field and GET /resourceful/entity/client-v2/:entity_id/:list_name/:list_obj_index/attachment/:field/:attach_id
(define-entrypoint entity/client-v2/<dynamic>/attachment :get
  (entity_id list_name list_obj_index field attach_id) ()
  :resource (format nil "/entity/client-v2/~A/~A/~A/attachment/~A~@[/~A~]"
		    entity_id list_name list_obj_index field attach_id)
  :documentation "With attach_id:
Display metadata for an attachment entry saved in the provided list object attachment field.
Otherwise:
List the attachments saved in the provided list object attachment field.")

;; entity/client-v2/<dynamic>/attachment - POST /resourceful/entity/client-v2/:entity_id/:list_name/:list_obj_index/attachment/:field
(define-entrypoint entity/client-v2/<dynamic>/attachment :post
  (entity_id list_name list_obj_index field) (filename data)
  :resource (format nil "/entity/client-v2/~A/~A/~A/attachment/~A" entity_id list_name list_obj_index field)
  :documentation "Upload a new attachment to a list object attachment field.")

;; entity/client-v2/<dynamic>/attachment - DELETE /resourceful/entity/client-v2/:entity_id/:list_name/:list_obj_index/attachment/:field/:attach_id
(define-entrypoint entity/client-v2/<dynamic>/attachment :delete
  (entity_id list_name list_obj_index field attach_id) ()
  :resource (format nil "/entity/client-v2/~A/~A/~A/attachment/~A/~A"
		    entity_id list_name list_obj_index field attach_id)
  :documentation "Remove an attachment from the list object attachment field.")

;; entity/client-v3/<dynamic>/attachment - GET /resourceful/entity/client-v3/:entity_id/:list_name/:list_obj_index/attachment/:field and GET /resourceful/entity/client-v3/:entity_id/:list_name/:list_obj_index/attachment/:field/:attach_id
(define-entrypoint entity/client-v3/<dynamic>/attachment :get
  (entity_id list_name list_obj_index field attach_id) ()
  :resource (format nil "/entity/client-v3/~A/~A/~A/attachment/~A~@[/~A~]"
		    entity_id list_name list_obj_index field attach_id)
  :documentation "With attach_id:
Display metadata for an attachment entry saved in the provided list object attachment field.
Otherwise:
List the attachments saved in the provided list object attachment field.")

;; entity/client-v3/<dynamic>/attachment - POST /resourceful/entity/client-v3/:entity_id/:list_name/:list_obj_index/attachment/:field
(define-entrypoint entity/client-v3/<dynamic>/attachment :post
  (entity_id list_name list_obj_index field) (filename data)
  :resource (format nil "/entity/client-v3/~A/~A/~A/attachment/~A" entity_id list_name list_obj_index field)
  :documentation "Upload a new attachment to a list object attachment field.")

;; entity/client-v3/<dynamic>/attachment - DELETE /resourceful/entity/client-v3/:entity_id/:list_name/:list_obj_index/attachment/:field/:attach_id
(define-entrypoint entity/client-v3/<dynamic>/attachment :delete
  (entity_id list_name list_obj_index field attach_id) ()
  :resource (format nil "/entity/client-v3/~A/~A/~A/attachment/~A/~A"
		    entity_id list_name list_obj_index field attach_id)
  :documentation "Remove an attachment from the list object attachment field.")
