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

File: src/api/ufield.lisp
Description: /ufield API Functions
|#

(in-package :cl-xplan-api/api)

;;; ufield

;; ufield

;; GET /resourceful/ufield/:group_name and GET /resourceful/ufield/:group_name/:field_name
(cl-xplan-api/core::define-entrypoint ufield :get
  (group_name field_name)
  ((fields :cond (and (not field_name) fields))
   ((include_choices nil choices-p) :cond (and (not field_name) choices-p) :value (if include_choices 1 0))
   ((include_dependencies nil dependant-p) :cond (and (not field_name) dependant-p) :value (if include_dependencies 1 0)))
  :resource (format nil "/ufield/~A~@[/~A~]" group_name field_name))
