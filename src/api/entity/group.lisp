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

File: src/api/entity/group.lisp
Description: /entity/group API functions
|#

(in-package :cl-xplan-api/api)

;; entity/group - GET /resourceful/entity/group and GET /resourceful/entity/group/:entity_id
(define-entrypoint entity/group :get
  (entity_id)
  (fields
   ((images_as_base64 nil images_as_base64-p) :cond images_as_base64-p
    :value (if images_as_base64 1 0))
   (ids :cond (and (not entity_id) ids))
   (quicksearch :cond (and (not entity_id) quicksearch))
   (parent_group_id :cond (and (not entity_id) parent_group_id))
   (group_name :cond (and (not entity_id) group_name))
   ((top_level_only nil top_level_only-p) :cond (and (not entity_id) top_level_only-p)
    :value (if top_level_only 1 0))
   (page_size :cond (and (not entity_id) page_size))
   (page_sort :cond (and (not entity_id) page_sort))
   (page_bookmark :cond (and (not entity_id) page_bookmark))
   (page_dir :cond (and (not entity_id) page_dir)))
  :resource (format nil "/entity/group~@[/~A~]" entity_id)
  :documentation "Returns all groups visible to the current session if entity_id is not provided otherwise returns the details of that group")

;; entity/group - POST /resourceful/entity/group
(define-entrypoint entity/group :post
  () (fields)
  :resource "/entity/group"
  :documentation "Create a new group entity by given group's details.")

;; entity/group - PATCH /resourceful/entity/group/:entity_id
(define-entrypoint entity/group :patch
  (entity_id) (fields)
  :resource (format nil "/entity/group/~A" entity_id)
  :documentation "Update the current group by given details.")

;; entity/group - DELETE /resourceful/entity/group/:entity_id
(define-entrypoint entity/group :delete
  (entity_id) ()
  :resource (format nil "/entity/group/~A" entity_id))
