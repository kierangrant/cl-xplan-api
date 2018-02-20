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

File: src/api/entity/client/action_to_proceed.lisp
Description: /entity/client/action_to_proceed API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints entity/client/action_to_proceed "action_to_proceed" "/entity/client")
(define-dynamiclike-entrypoints entity/client-v2/action_to_proceed "action_to_proceed" "/entity/client-v2")
(define-dynamiclike-entrypoints entity/client-v3/action_to_proceed "action_to_proceed" "/entity/client-v3")
