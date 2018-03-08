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

File: src/api/entity/client/client_group/member.lisp
Description: /entity/client/client_group/member API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints (entity/client/client_group/member
				 "client_group" "/entity/client" "member")
    :request-defaults (:inhibit-itemid t)
    :get-defaults (:inhibit-subitemid t :default-args nil)
    :post-defaults (:default-args
		       (member_id
			((login_accessible nil login_accessible-p)
			 :cond login_accessible-p :value (if login_accessible 1 0))))
    :put-defaults (:inhibit t)
    :patch-defaults (:sublist-obj-field member_id
		     :default-args (((login_accessible_only nil login_accessible_only-p)
				     :cond login_accessible_only-p
				     :value (if login_accessible_only 1 0))))
    :delete-defaults (:sublist-obj-field member_id))

(define-dynamiclike-entrypoints (entity/client-v2/client_group/member
				 "client_group" "/entity/client-v2" "member")
    :request-defaults (:inhibit-itemid t)
    :get-defaults (:inhibit-subitemid t :default-args nil)
    :post-defaults (:default-args
		       (member_id
			((login_accessible nil login_accessible-p)
			 :cond login_accessible-p :value (if login_accessible 1 0))))
    :put-defaults (:inhibit t)
    :patch-defaults (:sublist-obj-field member_id
		     :default-args (((login_accessible_only nil login_accessible_only-p)
				     :cond login_accessible_only-p
				     :value (if login_accessible_only 1 0))))
    :delete-defaults (:sublist-obj-field member_id))

(define-dynamiclike-entrypoints (entity/client-v3/client_group/member
				 "client_group" "/entity/client-v3" "member")
    :request-defaults (:inhibit-itemid t)
    :get-defaults (:inhibit-subitemid t :default-args nil)
    :post-defaults (:default-args
		       (member_id
			((login_accessible nil login_accessible-p)
			 :cond login_accessible-p :value (if login_accessible 1 0))))
    :put-defaults (:inhibit t)
    :patch-defaults (:sublist-obj-field member_id
		     :default-args (((login_accessible_only nil login_accessible_only-p)
				     :cond login_accessible_only-p
				     :value (if login_accessible_only 1 0))))
    :delete-defaults (:sublist-obj-field member_id))
