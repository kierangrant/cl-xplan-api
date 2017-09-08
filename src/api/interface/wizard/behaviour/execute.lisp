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

File: src/api/interface/wizard/behaviour/execute.lisp
Description: /interface/wizard/behaviour/execute API functions
|#

(in-package :cl-xplan-api/api)

;; interface/wizard/behaviour/execute - POST /resourceful/interface/:interface/wizard/:wizard/behaviour/execute/:entity_id
(define-entrypoint interface/wizard/behaviour/execute :post
  (interface wizard entity_id) (direction)
  :documentation "This will execute automatic actions configured to run on a wizard page event.

The most common event configured is the completion of a wizard, but actions may also run on cancellation or \"save and exit\" of a wizard.

Example actions are:
    Set an entity field
    Create a task"
  :resource (format nil "/interface/~A/wizard/~A/behaviour/execute/~A" interface wizard entity_id))
