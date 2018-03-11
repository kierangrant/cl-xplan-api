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

File: src/api/entity/client/company_keyperson.lisp
Description: /entity/client/company_keyperson API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/company_keyperson "company_keyperson" "/entity/client")
    :request-defaults (:list-obj-field keyperson_id)
    :get-defaults (:default-args ((fields :cond (and (not keyperson_id) fields))))
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints (entity/client-v2/company_keyperson "company_keyperson" "/entity/client-v2")
    :request-defaults (:list-obj-field keyperson_id)
    :get-defaults (:default-args ((fields :cond (and (not keyperson_id) fields))))
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints (entity/client-v3/company_keyperson "company_keyperson" "/entity/client-v3")
    :request-defaults (:list-obj-field keyperson_id)
    :get-defaults (:default-args ((fields :cond (and (not keyperson_id) fields))))
    :put-defaults (:inhibit t))
