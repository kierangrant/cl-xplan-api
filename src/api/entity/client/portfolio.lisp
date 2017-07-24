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

File: src/api/entity/client/portfolio.lisp
Description: /entity/client/portfolio API Functions
|#

(in-package :cl-xplan-api/api)

;; entity/client/portfolio - GET /resourceful/entity/client/:entity_id/portfolio
(define-entrypoint entity/client/portfolio :get
  (entity_id) (fields)
  :resource (format NIL "/entity/client/~A/portfolio" entity_id))

;; entity/client-v2/portfolio - GET /resourceful/entity/client-v2/:entity_id/portfolio
(define-entrypoint entity/client-v2/portfolio :get
  (entity_id) (fields)
  :resource (format NIL "/entity/client-v2/~A/portfolio" entity_id))

;; entity/client-v3/portfolio - GET /resourceful/entity/client-v3/:entity_id/portfolio
(define-entrypoint entity/client-v3/portfolio :get
  (entity_id) (fields)
  :resource (format NIL "/entity/client-v3/~A/portfolio" entity_id))
