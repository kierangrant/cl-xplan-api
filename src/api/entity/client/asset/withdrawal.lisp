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

File: src/api/entity/client/asset/withdrawal.lisp
Description: /entity/client/asset/withdrawal API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/asset/withdrawal "asset" "/entity/client" "withdrawal")
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints (entity/client-v2/asset/withdrawal "asset" "/entity/client-v2" "withdrawal")
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints (entity/client-v3/asset/withdrawal "asset" "/entity/client-v3" "withdrawal")
    :put-defaults (:inhibit t))
