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

File: src/api/docnote/category.lisp
Description: /docnote/category API Functions
|#

(in-package :cl-xplan-api/api)

;;; docnote/category

;; docnote/category - GET /resourceful/docnote/category and GET /resourceful/docnote/category/:catid
(define-entrypoint docnote/category :get (catid) ((parentid :cond (and (not catid) parentid)))
		   :resource (format nil "/docnote/category~@[/~A~]" catid))

;; docnote/category - POST /resourceful/docnote/category
(define-entrypoint docnote/category :post
  ()
  (parentid referrer_permission edit_privilege feed_permission
	    ((is_publishable nil publishable-p) :cond publishable-p :value (if is_publishable 1 0))
	    name permission)
  :resource "/docnote/category")

;; docnote/category - PATCH /resourceful/docnote/category/:catid
(define-entrypoint docnote/category :patch
  (catid)
  (parentid referrer_permission edit_privilege feed_permission
	    ((is_publishable nil publishable-p) :cond publishable-p :value (if is_publishable 1 0))
	    name permission)
  :resource (format nil "/docnote/category/~A" catid))

;; docnote/category - DELETE /resourceful/docnote/category/:catid
(define-entrypoint docnote/category :delete (catid) ()
		   :resource (format nil "/docnote/category/~A" catid))

;;; docnote-v2/category

;; docnote-v2/category - GET /resourceful/docnote-v2/category and GET /resourceful/docnote-v2/category/:catid
(define-entrypoint docnote-v2/category :get (catid) ((parentid :cond (and (not catid) parentid)))
		   :resource (format nil "/docnote-v2/category~@[/~A~]" catid))

;; docnote-v2/category - POST /resourceful/docnote-v2/category
(define-entrypoint docnote-v2/category :post
  ()
  (parentid referrer_permission edit_privilege feed_permission
	    ((is_publishable nil publishable-p) :cond publishable-p :value (if is_publishable 1 0))
	    name permission)
  :resource "/docnote-v2/category")

;; docnote-v2/category - PATCH /resourceful/docnote-v2/category/:catid
(define-entrypoint docnote-v2/category :patch
  (catid)
  (parentid referrer_permission edit_privilege feed_permission
	    ((is_publishable nil publishable-p) :cond publishable-p :value (if is_publishable 1 0))
	    name permission)
  :resource (format nil "/docnote-v2/category/~A" catid))

;; docnote-v2/category - DELETE /resourceful/docnote-v2/category/:catid
(define-entrypoint docnote-v2/category :delete (catid) ()
		   :resource (format nil "/docnote-v2/category/~A" catid))
