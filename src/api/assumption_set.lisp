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

File: src/api/assumption_set.lisp
Description: /assumption_set API Functions
|#

(in-package :cl-xplan-api/api)

;; assumption_set - GET /resourceful/assumption_set and GET /resourceful/assumption_set/:assumption_set_name

(define-entrypoint assumption_set :get
  (assumption_set_name) ()
  :resource (format NIL "/assumption_set~@[/~A~]" assumption_set_name))
