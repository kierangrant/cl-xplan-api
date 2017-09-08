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

File: src/api/interface.lisp
Description: /interface API functions
|#

(in-package :cl-xplan-api/api)

;; interface - GET /resourceful/interface and GET /resourceful/interface/:interface
(define-entrypoint interface :get
  (interface) ()
  :documentation "If interface is provided:
Fetch the menu tree hierarchy as configured in Edit Interface.

Note this is NOT the resolved per-entity menu list which applies menu post-processing such as visibility and dynamic submenus.

Otherwise there is no documentation for the no-parameter version of this function."
  :resource (format nil "/interface~@[/~A~]" interface))
