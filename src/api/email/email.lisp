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

File: src/api/email/email.lisp
Description: /email/email API functions
|#

(in-package :cl-xplan-api/api)

;; email/email - POST /resourceful/email/email?_method=send
(define-entrypoint email/email :send
  ((note_param.add_to_note nil add_to_note-p)
   (note_param.note_accessible nil note_accessible-p)
   note_param.note_type
   note_param.note_subtype
   note_param.permission
   note_param.groups
   note_param.note_container_tpl_id)
  (((send_now nil send_now-p) :cond send_now-p :value (if send_now 1 0))
   template subject content from_address to cc bcc send_when other_fromaddress attached_entities recipients
   attachments
   ((note_param nil note_param-p) :cond T
    :value
    (if note_param-p note_param
	(cond-hash
	  (add_to_note-p "add_to_note" (if note_param.add_to_note 1 0))
	  (note_accessible-p "note_accessible" (if note_param.note_accessible 1 0))
	  (note_param.note_type "note_type")
	  (note_param.note_subtype "note_subtype")
	  (note_param.permission "permission")
	  (note_param.groups "groups")
	  (note_param.note_container_tpl_id "note_container_tpl_id")))))
  :documentation
  "to, cc, bcc and attachments are array of objects, please construct using hash-tables, a-lists or p-lists"
  :single-method :post
  :single-resource "/email/email?_method=send"
  :bulk-resource "/email/email"
  :single-parms-as-body T)

