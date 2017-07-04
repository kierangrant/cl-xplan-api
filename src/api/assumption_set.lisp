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

;;; assumption_set

;; assumption_set

;; GET /resourceful/assumption_set and GET /resourceful/assumption_set/:assumption_set_name

(cl-xplan-api/core::define-entrypoint assumption_set :get
  (assumption_set_name)
  ()
  :resource (format NIL "/assumption_set~@[/~A~]" assumption_set_name))

;; assumption_set/risk_profile

;; GET /resourceful/assumption_set/:assumption_set_name/risk_profile and GET /resourceful/assumption_set/:assumption_set_name/risk_profile/:risk_profile

(cl-xplan-api/core::define-entrypoint assumption_set/risk_profile :get
  (assumption_set_name risk_profile) ()
  :resource (format NIL "/assumption_set/~A/risk_profile~@[/~A~]" assumption_set_name risk_profile))

;; assumption_set/risk_profile/asset_class

;; GET /resourceful/assumption_set/:assumption_set_name/risk_profile/:risk_profile/asset_class and GET /resourceful/assumption_set/:assumption_set_name/risk_profile/:risk_profile/asset_class/:asset_class_id

(cl-xplan-api/core::define-entrypoint assumption_set/risk_profile/asset_class :get
  (assumption_set_name risk_profile asset_class_id) ()
  :resource (format NIL "/assumption_set/~A/risk_profile/~A/asset_class~@[/~A~]"
		    assumption_set_name risk_profile asset_class_id))
