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

;; POST /resourceful/email/email?_method=send
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
	(let ((h (make-hash-table)))
	  (if add_to_note-p (setf (gethash "add_to_note" h) (if note_param.add_to_note 1 0)))
	  (if note_accessible-p (setf (gethash "note_accessible" h) (if note_param.note_accessible 1 0)))
	  (if note_param.note_type (setf (gethash "note_type" h) note_param.note_type))
	  (if note_param.note_subtype (setf (gethash "note_subtype" h) note_param.note_subtype))
	  (if note_param.permission (setf (gethash "permission" h) note_param.permission))
	  (if note_param.groups (setf (gethash "groups" h) note_param.groups))
	  (if note_param.note_container_tpl_id (setf (gethash "note_container_tpl_id" h)
						     note_param.note_container_tpl_id))
	  h))))
  :documentation
  "to, cc, bcc and attachments are array of objects, please construct using hash-tables, a-lists or p-lists"
  :single-method :post
  :single-resource "/email/email?_method=send"
  :bulk-resource "/email/email"
  :single-parms-as-body T)

