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

File: src/api/interface/visibility.lisp
Description: /interface/visibility API functions
|#

(in-package :cl-xplan-api/api)

;; interface/visibility - GET /resourceful/interface/:interface/visibility/:entity_id
(define-entrypoint interface/visibility :get
  (interface entity_id) (paths)
  :resource (format nil "/interface/~A/visibility/~A" interface entity_id))
