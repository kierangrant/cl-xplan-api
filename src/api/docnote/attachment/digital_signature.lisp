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

File: src/api/docnote/attachment/digital_signature.lisp
Description: /docnote/attachment/digital_signature API functions
|#

(in-package :cl-xplan-api/api)

;; docnote/attachment-v3/digital_signature - GET /resourceful/docnote/:docid/attachment-v3/:docpartid/digital_signature
(define-entrypoint docnote/attachment-v3/digital_signature :get
  (docid docpartid) ()
  :resource (format nil "/docnote/~A/attachment-v3/~A/digital_signature" docid docpartid))

;; docnote/attachment-v3/digital_signature - POST /resourceful/docnote/:docid/attachment-v3/:docpartid/digital_signature?_method=post
(define-entrypoint docnote/attachment-v3/digital_signature :post
  (docid docpartid) (due_in_days)
  :single-resource (format nil "/docnote/~A/attachment-v3/~A/digital_signature?_method=post" docid docpartid)
  :bulk-resource (format nil "/docnote/~A/attachment-v3/~A/digital_signature" docid docpartid))

;; docnote-v2/attachment-v3/digital_signature - GET /resourceful/docnote-v2/:docid/attachment-v3/:docpartid/digital_signature
(define-entrypoint docnote-v2/attachment-v3/digital_signature :get
  (docid docpartid) ()
  :resource (format nil "/docnote-v2/~A/attachment-v3/~A/digital_signature" docid docpartid))

;; docnote-v2/attachment-v3/digital_signature - POST /resourceful/docnote-v2/:docid/attachment-v3/:docpartid/digital_signature?_method=post
(define-entrypoint docnote-v2/attachment-v3/digital_signature :post
  (docid docpartid) (due_in_days)
  :single-resource (format nil "/docnote-v2/~A/attachment-v3/~A/digital_signature?_method=post" docid docpartid)
  :bulk-resource (format nil "/docnote-v2/~A/attachment-v3/~A/digital_signature" docid docpartid))
