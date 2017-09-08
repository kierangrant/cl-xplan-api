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

File: src/api/session/user/adviser.lisp
Description: /session/user/adviser API functions
|#

(in-package :cl-xplan-api/api)

;; session/user/adviser - GET /resourceful/session/user/adviser and GET /resourceful/session/user/adviser/:entity_id
(define-entrypoint session/user/adviser :get
  (entity_id)
  ((fields :cond (and (not entity_id) fields))
   ((images_as_base64 nil images_as_base64-p)
    :cond (and (not entity_id) images_as_base64-p)
    :value (if images_as_base64 1 0)))
  :resource (format nil "/session/user/adviser~@[/~A~]" entity_id)
  :documentation "Retrieve information about client advisers, if an entity_id is given: Restricted access to adviser fields. Allowed fields: first_name, last_name and photo")
