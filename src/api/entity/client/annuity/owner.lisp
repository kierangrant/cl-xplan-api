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

File: src/api/entity/client/annuity/owner.lisp
Description: /entity/client/annuity/owner API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/annuity/owner "annuity" "/entity/client" "owner")
    :request-defaults (:inhibit t :inhibit-subitemid t)
    :get-defaults (:default-args nil :inhibit nil)
    :put-defaults (:default-args (percentage owner_id) :inhibit nil))

(define-dynamiclike-entrypoints (entity/client-v2/annuity/owner "annuity" "/entity/client-v2" "owner")
    :request-defaults (:inhibit t :inhibit-subitemid t)
    :get-defaults (:default-args nil :inhibit nil)
    :put-defaults (:default-args (percentage owner_id) :inhibit nil))

(define-dynamiclike-entrypoints (entity/client-v3/annuity/owner "annuity" "/entity/client-v3" "owner")
    :request-defaults (:inhibit t :inhibit-subitemid t)
    :get-defaults (:default-args nil :inhibit nil)
    :put-defaults (:default-args (percentage owner_id) :inhibit nil))
