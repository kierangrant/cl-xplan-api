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

File: src/api/sticky_note.lisp
Description: /sticky_note API functions
|#

(in-package :cl-xplan-api/api)

;; sticky_note - GET /resourceful/sticky_note and  GET /resourceful/sticky_note/:note_id
(define-entrypoint sticky_note :get
  (note_id) ((page_id :cond (and page_id (not note_id))) (entity_id :cond (and entity_id (not note_id))))
  :resource (format nil "/sticky_note~@[/~A~]" note_id))

;; sticky_note - POST /resourceful/sticky_note
(define-entrypoint sticky_note :post
  (position.top position.left size.width size.height)
  (title content page_id ((private nil private-p) :cond private-p :value (if private 1 0))
	 (position
	  :cond (or position position.top position.left)
	  :value
	  (if position position
	      (let ((h (make-hash-table)))
		(if position.top (setf (gethash "top" h) position.top))
		(if position.left (setf (gethash "left" h) position.left))
		h)))
	 (size
	  :cond (or size size.width size.height)
	  :value
	  (if size size
	      (let ((h (make-hash-table)))
		(if size.width (setf (gethash "width" h) size.width))
		(if size.height (setf (gethash "height" h) size.height))
		h)))
	 colour
	 ((warning nil warning-p) :cond warning-p :value (if warning 1 0))
	 entity_id)
  :resource "/sticky_note")

;; sticky_note - PATCH /resourceful/sticky_note/:note_id
(define-entrypoint sticky_note :patch
  (note_id position.top position.left size.width size.height)
  (title content page_id ((private nil private-p) :cond private-p :value (if private 1 0))
	 (position
	  :cond (or position position.top position.left)
	  :value
	  (if position position
	      (let ((h (make-hash-table)))
		(if position.top (setf (gethash "top" h) position.top))
		(if position.left (setf (gethash "left" h) position.left))
		h)))
	 (size
	  :cond (or size size.width size.height)
	  :value
	  (if size size
	      (let ((h (make-hash-table)))
		(if size.width (setf (gethash "width" h) size.width))
		(if size.height (setf (gethash "height" h) size.height))
		h)))
	 colour
	 ((warning nil warning-p) :cond warning-p :value (if warning 1 0)))
  :resource (format nil "/sticky_note/~A" note_id))

;; sticky_note - DELETE /resourceful/sticky_note/:note_id
(define-entrypoint sticky_note :delete
  (note_id) ()
  :resource (format nil "/sticky_note/~A" note_id))
