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

File: src/api/entity/client/capability.lisp
Description: /entity/client/capability API functions
|#

(in-package :cl-xplan-api/api)

;; entity/client/capability - GET /resourceful/entity/client/:entity_id/capability
(define-entrypoint entity/client/capability :get (entity_id) (capabilities)
		   :resource (format nil "/entity/client/~A/capability" entity_id))

;; entity/client/capability - GET /resourceful/entity/client/:entity_id/capability
(define-entrypoint entity/client-v2/capability :get (entity_id) (capabilities)
		   :resource (format nil "/entity/client-v2/~A/capability" entity_id))

;; entity/client/capability - GET /resourceful/entity/client/:entity_id/capability
(define-entrypoint entity/client-v3/capability :get (entity_id) (capabilities)
		   :resource (format nil "/entity/client-v3/~A/capability" entity_id))
