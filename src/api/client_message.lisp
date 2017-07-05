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

File: src/api/client_message.lisp
Description: /client_message API Functions
|#

(in-package :cl-xplan-api/api)

;;; client_message

;; client_message - POST /resourceful/client_message
(cl-xplan-api/core::define-entrypoint client_message :post
  () (subject content adviser_id attachments attachment_names)
  :resource "/client_message" :documentation "Send a message to the client's adviser.")
