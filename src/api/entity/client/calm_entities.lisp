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

File: src/api/entity/client/calm_entities.lisp
Description: /entity/client/calm_entities API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/calm_entities "calm_entities" "/entiy/client")
    :put-defaults (:inhibit t))
(define-dynamiclike-entrypoints (entity/client-v2/calm_entities "calm_entities" "/entiy/client-v2")
    :put-defaults (:inhibit t))
(define-dynamiclike-entrypoints (entity/client-v3/calm_entities "calm_entities" "/entiy/client-v3")
    :put-defaults (:inhibit t))
