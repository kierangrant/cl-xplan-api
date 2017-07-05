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

File: src/api/class_smsf.lisp
Description: /class_smsf API Functions
|#

(in-package :cl-xplan-api/api)

;;; class_smsf/account

;; class_smsf/account - GET /resourceful/class_smsf/account and GET /resourceful/class_smsf/account/:username
(cl-xplan-api/core::define-entrypoint class_smsf/account :get
  (username) ((passwd :cond (and username passwd)))
  :resource (format NIL "/class_smsf/account~@[/~A~]" username))

;; class_smsf/account - POST /resourceful/class_smsf/account
(cl-xplan-api/core::define-entrypoint class_smsf/account :post
  () (username password) :resource "/class_smsf/account")

;; class_smsf/account - PUT /resourceful/class_smsf/account/:username
(cl-xplan-api/core::define-entrypoint class_smsf/account :put
  (username) (passwd brand) :resource (format nil "/class_smsf/account/~A" username))

;;; class_smsf/fund

;; class_smsf/fund - GET /resourceful/class_smsf/fund and GET /resourceful/class_smsf/fund/:fund
(cl-xplan-api/core::define-entrypoint class_smsf/fund :get
  (fund) () :resource (format nil "/class_smsf/fund~@[/~A~]" fund)
  :documentation "If fund isn't provided is 'Collection handler under portfolio /class_smsf/fund' otherwise it is 'Link handler under class_smsf /class_smsf/fund/<fund_code>'")

;;; class_smsf/fund/contribution_caps

;; class_smsf/fund/contribution_caps - GET /resourceful/class_smsf/fund/:fund/contribution_caps
(cl-xplan-api/core::define-entrypoint class_smsf/fund/contribution_caps :get
  (fund) (year) :resource (format nil "/class_smsf/fund/~A/contribution_caps" fund))

;;; class_smsf/fund/members

;; class_smsf/fund/members
(cl-xplan-api/core::define-entrypoint class_smsf/fund/members :get
  (fund) () :resource (format nil "/class_smsf/fund/~A/members" fund))

;;; class_smsf/fund_link

;; class_smsf/fund_link - GET /resourceful/class_smsf/fund_link and GET /resourceful/class_smsf/fund_link/:portfolioid
(cl-xplan-api/core::define-entrypoint class_smsf/fund_link :get
  (portfolioid) ()
  :documentation "Without portfolioid is 'Collection handler under portfolio /class_smsf/fund_link' otherwise is 'Link handler under class_smsf /class_smsf/fund_link/<portfolioid>'"
  :resource (format nil "/class_smsf/fund_link~@[/~A~]" portfolioid))

;; class_smsf/fund_link - PUT /resourceful/class_smsf/fund_link/:portfolioid
(cl-xplan-api/core::define-entrypoint class_smsf/fund_link :put
  (portfolioid) (fund_code) :resource (format nil "/class_smsf/fund_link/~A" portfolioid))
