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

File: src/api/entity/group/user.lisp
Description: /entity/group/user API functions
|#

(in-package :cl-xplan-api/api)

;; entity/group/user - GET /resourceful/entity/group/:entity_id/user and GET /resourceful/entity/group/:entity_id/user/:linked_entity_id
(define-entrypoint entity/group/user :get
  (entity_id linked_entity_id) ()
  :resource (format nil "/entity/group/~A/user~@[/~A~]" entity_id linked_entity_id)
  :documentation "Show all the users linked to the group if linked_entity_id is not provided otherwise Return details if the given user is linked to the group.")

;; entity/group/user - PUT /resourceful/entity/group/:entity_id/user
(define-entrypoint entity/group/user :put
  (entity_id) (linked_entity_id)
  :resource (format nil "/entity/group/~A/user" entity_id)
  :documentation "Add a new user to the group.")

;; entity/group/user - DELETE /resourceful/entity/group/:entity_id/user/:linked_entity_id
(define-entrypoint entity/group/user :delete
  (entity_id linked_entity_id) ()
  :resource (format nil "/entity/group/~A/user/~A" entity_id linked_entity_id))
