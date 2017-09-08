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

File: src/api/interface/wizard/subpage/visibility.lisp
Description: /interface/wizard/subpage/visibility API functions
|#

(in-package :cl-xplan-api/api)

;; interface/wizard/subpage/visibility - GET /resourceful/interface/:interface/wizard/:wizard/subpage/visibility/:entity_id
(define-entrypoint interface/wizard/subpage/visibility :get
  (interface wizard entity_id) ()
  :documentation "Wizard subpage visibility"
  :resource (format nil "/interface/~A/wizard/~A/subpage/visibility/~A" interface wizard entity_id))
