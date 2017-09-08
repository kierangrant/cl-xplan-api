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

File: src/api/ufield/choice_category_dependence.lisp
Description: /ufield/choice_category_dependence API functions
|#

(in-package :cl-xplan-api/api)

;; ufield/choice_category_dependence - GET /resourceful/ufield/:group_name/:field_name/choice_category_dependence
(define-entrypoint ufield/choice_category_dependence :get
  (group_name field_name) ()
  :documentation "A dictionary of choice category dependence This method is only available for ufields with type ft_choice."
  :resource (format nil "ufield/~A/~A/choice_category_dependence" group_name field_name))
