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

File: src/api/entity/client/asset.lisp
Description: /entity/client/asset API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints
    (entity/client/asset "asset" "/entity/client")
    :post-defaults
  (:extra-args (security_id ((create_position_associate nil create_position_associate-p) :cond create_position_associate-p :value (if create_position_associate 1 0))))
  :patch-defaults
  (:extra-args (security_id ((create_position_associate nil create_position_associate-p) :cond create_position_associate-p :value (if create_position_associate 1 0))))
  :put-defaults (:inhibit T))

(define-dynamiclike-entrypoints
    (entity/client-v2/asset "asset" "/entity/client-v2")
    :post-defaults
  (:extra-args (security_id ((create_position_associate nil create_position_associate-p) :cond create_position_associate-p :value (if create_position_associate 1 0))))
  :patch-defaults
  (:extra-args (security_id ((create_position_associate nil create_position_associate-p) :cond create_position_associate-p :value (if create_position_associate 1 0))))
  :put-defaults (:inhibit T))

(define-dynamiclike-entrypoints
    (entity/client-v3/asset "asset" "/entity/client-v3")
    :post-defaults
  (:extra-args (security_id ((create_position_associate nil create_position_associate-p) :cond create_position_associate-p :value (if create_position_associate 1 0))))
  :patch-defaults
  (:extra-args (security_id ((create_position_associate nil create_position_associate-p) :cond create_position_associate-p :value (if create_position_associate 1 0))))
  :put-defaults (:inhibit T))
