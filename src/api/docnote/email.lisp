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

File: src/api/docnote/email.lisp
Description: /docnote/email API Functions
|#

(in-package :cl-xplan-api/api)

;; docnote-v2/email - POST /resourceful/docnote-v2/:docid/email?_method=send
(define-entrypoint docnote-v2/email :send
  (docid) (email_address template)
  :single-resource (format nil "/docnote-v2/~A/email?_method=send" docid)
  :single-method :post
  :single-parms-as-body T
  :bulk-resource (format nil "/docnote-v2/~A/email" docid)
  :bulk-method :send
  :documentation "Send an email based on a specified email template. Attachments on the note will be sent as email attachments")
