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

File: src/api/docnote/firstread.lisp
Description: /docnote/firstread API Functions
|#

(in-package :cl-xplan-api/api)

;; docnote-v2/firstread - GET /resourceful/docnote-v2/:docid/firstread and GET /resourceful/docnote-v2/:docid/firstread/:entityid
(define-entrypoint docnote-v2/firstread :get
  (docid entityid) ()
  :resource (format nil "/docnote-v2/~A/firstread~@[/~A~]" docid entityid))

;; docnote-v2/firstread - POST /resourceful/docnote-v2/:docid/firstread
(define-entrypoint docnote-v2/firstread :post
  (docid) ()
  :resource (format nil "/docnote-v2/~A/firstread" docid))
