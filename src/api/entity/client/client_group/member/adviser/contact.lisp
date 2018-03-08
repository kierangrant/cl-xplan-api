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

File: src/api/entity/client/client_group/member/adviser/contact.lisp
Description: /entity/client/client_group/member/adviser/contact API functions
|#

(in-package :cl-xplan-api/api)

;; entity/client/client_group/member/adviser/contact - GET /resourceful/entity/client/:entity_id/client_group/member/:member_id/adviser/:adviser_id/contact
(define-entrypoint entity/client/client_group/member/adviser/contact :get
  (entity_id member_id adviser_id) (index fields page)
  :resource (format nil "/entity/client/~A/client_group/member/~A/adviser/~A/contact"
		    entity_id member_id adviser_id))

;; entity/client-v2/client_group/member/adviser/contact - GET /resourceful/entity/client-v2/:entity_id/client_group/member/:member_id/adviser/:adviser_id/contact
(define-entrypoint entity/client-v2/client_group/member/adviser/contact :get
  (entity_id member_id adviser_id) (index fields page)
  :resource (format nil "/entity/client-v2/~A/client_group/member/~A/adviser/~A/contact"
		    entity_id member_id adviser_id))

;; entity/client-v3/client_group/member/adviser/contact - GET /resourceful/entity/client-v3/:entity_id/client_group/member/:member_id/adviser/:adviser_id/contact
(define-entrypoint entity/client-v3/client_group/member/adviser/contact :get
  (entity_id member_id adviser_id) (index fields page)
  :resource (format nil "/entity/client-v3/~A/client_group/member/~A/adviser/~A/contact"
		    entity_id member_id adviser_id))
