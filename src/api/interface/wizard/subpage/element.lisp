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

File: src/api/interface/wizard/subpage/element.lisp
Description: /interface/wizard/subpage/element API functions
|#

(in-package :cl-xplan-api/api)

;; interface/wizard/subpage/element - GET /resourceful/interface/:interface/wizard/:wizard/subpage/:subpage/element and GET /resourceful/interface/:interface/wizard/:wizard/subpage/:subpage/element/:index
(define-entrypoint interface/wizard/subpage/element :get
  (interface wizard subpage index) ()
  :documentation "List of configured interface elements for a page."
  :resource (format nil "/interface/~A/wizard/~A/subpage/~A/element~@[/~A~]" interface wizard subpage index))
