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

File: src/api/docnote/attachment.lisp
Description: /docnote/attachment API Functions
|#

(in-package :cl-xplan-api/api)

;;; docnote/attachment

;; docnote/attachment - GET /resourceful/docnote/:docid/attachment and GET /resourceful/docnote/:docid/attachment/:docpartid
(define-entrypoint docnote/attachment :get (docid docpartid) ()
		   :resource (format nil "/docnote/~A/attachment~@[/~A~]" docid docpartid))

;; docnote/attachment - POST /resourceful/docnote/:docid/attachment
(define-entrypoint docnote/attachment :post (docid) (filename mimetype metadata content)
		   :resource (format nil "/docnote/~A/attachment" docid))

;; docnote/attachment - PATCH /resourceful/docnote/:docid/attachment/:docpartid
(define-entrypoint docnote/attachment :patch (docid docpartid) (filename mimetype metadata content)
		   :resource (format nil "/docnote/~A/attachment/~A" docid docpartid))

;; docnote/attachment - DELETE /resourceful/docnote/:docid/attachment/:docpartid
(define-entrypoint docnote/attachment :delete (docid docpartid) ()
		   :resource (format nil "/docnote/~A/attachment/~A" docid docpartid))
;;; docnote-v2/attachment

;; docnote-v2/attachment - GET /resourceful/docnote-v2/:docid/attachment and GET /resourceful/docnote-v2/:docid/attachment/:docpartid
(define-entrypoint docnote-v2/attachment :get (docid docpartid) ()
		   :resource (format nil "/docnote-v2/~A/attachment~@[/~A~]" docid docpartid))

;; docnote-v2/attachment - POST /resourceful/docnote-v2/:docid/attachment
(define-entrypoint docnote-v2/attachment :post (docid) (filename mimetype metadata content)
		   :resource (format nil "/docnote-v2/~A/attachment" docid))

;; docnote-v2/attachment - PATCH /resourceful/docnote-v2/:docid/attachment/:docpartid
(define-entrypoint docnote-v2/attachment :patch (docid docpartid) (filename mimetype metadata content)
		   :resource (format nil "/docnote-v2/~A/attachment/~A" docid docpartid))

;; docnote-v2/attachment - DELETE /resourceful/docnote-v2/:docid/attachment/:docpartid
(define-entrypoint docnote-v2/attachment :delete (docid docpartid) ()
		   :resource (format nil "/docnote-v2/~A/attachment/~A" docid docpartid))

;;; docnote/attachment-v2

;; docnote/attachment-v2 - GET /resourceful/docnote/:docid/attachment-v2 and GET /resourceful/docnote/:docid/attachment-v2/:docpartid
(define-entrypoint docnote/attachment-v2 :get (docid docpartid) ()
		   :resource (format nil "/docnote/~A/attachment-v2~@[/~A~]" docid docpartid))

;; docnote/attachment-v2 - POST /resourceful/docnote/:docid/attachment-v2
(define-entrypoint docnote/attachment-v2 :post (docid) (filename mimetype metadata content)
		   :resource (format nil "/docnote/~A/attachment-v2" docid))

;; docnote/attachment-v2 - PATCH /resourceful/docnote/:docid/attachment-v2/:docpartid
(define-entrypoint docnote/attachment-v2 :patch (docid docpartid) (filename mimetype metadata content)
		   :resource (format nil "/docnote/~A/attachment-v2/~A" docid docpartid))

;; docnote/attachment-v2 - DELETE /resourceful/docnote/:docid/attachment-v2/:docpartid
(define-entrypoint docnote/attachment-v2 :delete (docid docpartid) ()
		   :resource (format nil "/docnote/~A/attachment-v2/~A" docid docpartid))

;;; docnote-v2/attachment-v2

;; docnote-v2/attachment-v2 - GET /resourceful/docnote-v2/:docid/attachment-v2 and GET /resourceful/docnote-v2/:docid/attachment-v2/:docpartid
(define-entrypoint docnote-v2/attachment-v2 :get (docid docpartid) ()
		   :resource (format nil "/docnote-v2/~A/attachment-v2~@[/~A~]" docid docpartid))

;; docnote-v2/attachment-v2 - PATCH /resourceful/docnote-v2/:docid/attachment-v2/:docpartid
(define-entrypoint docnote-v2/attachment-v2 :patch (docid docpartid)
		   (filename mimetype metadata content)
		   :resource (format nil "/docnote-v2/~A/attachment-v2/~A" docid docpartid))

;; docnote-v2/attachment-v2 - DELETE /resourceful/docnote-v2/:docid/attachment-v2/:docpartid
(define-entrypoint docnote-v2/attachment-v2 :delete (docid docpartid) ()
		   :resource (format nil "/docnote-v2/~A/attachment-v2/~A" docid docpartid))

;;; docnote/attachment-v3

;; docnote/attachment-v3 - GET /resourceful/docnote/:docid/attachment-v3 and GET /resourceful/docnote/:docid/attachment-v3/:docpartid
(define-entrypoint docnote/attachment-v3 :get (docid docpartid) ()
		   :resource (format nil "/docnote/~A/attachment-v3~@[/~A~]" docid docpartid))

;; docnote/attachment-v3 - POST /resourceful/docnote/:docid/attachment-v3
(define-entrypoint docnote/attachment-v3 :post (docid) (filename mimetype metadata content)
		   :resource (format nil "/docnote/~A/attachment-v3" docid))

;; docnote/attachment-v3 - PATCH /resourceful/docnote/:docid/attachment-v3/:docpartid
(define-entrypoint docnote/attachment-v3 :patch (docid docpartid) (filename mimetype metadata content)
		   :resource (format nil "/docnote/~A/attachment-v3/~A" docid docpartid))

;; docnote/attachment-v3 - DELETE /resourceful/docnote/:docid/attachment-v3/:docpartid
(define-entrypoint docnote/attachment-v3 :delete (docid docpartid) ()
		   :resource (format nil "/docnote/~A/attachment-v3/~A" docid docpartid))

;;; docnote-v2/attachment-v3

;; docnote-v2/attachment-v3 - GET /resourceful/docnote-v2/:docid/attachment-v3 and GET /resourceful/docnote-v2/:docid/attachment-v3/:docpartid
(define-entrypoint docnote-v2/attachment-v3 :get (docid docpartid) ()
		   :resource (format nil "/docnote-v2/~A/attachment-v3~@[/~A~]" docid docpartid))

;; docnote-v2/attachment-v3 - POST /resourceful/docnote-v2/:docid/attachment-v3
(define-entrypoint docnote-v2/attachment-v3 :post (docid) (filename mimetype metadata content)
		   :resource (format nil "/docnote-v2/~A/attachment-v3" docid))

;; docnote-v2/attachment-v3 - PATCH /resourceful/docnote-v2/:docid/attachment-v3/:docpartid
(define-entrypoint docnote-v2/attachment-v3 :patch (docid docpartid)
		   (filename mimetype metadata content)
		   :resource (format nil "/docnote-v2/~A/attachment-v3/~A" docid docpartid))

;; docnote-v2/attachment-v3 - DELETE /resourceful/docnote-v2/:docid/attachment-v3/:docpartid
(define-entrypoint docnote-v2/attachment-v3 :delete (docid docpartid) ()
		   :resource (format nil "/docnote-v2/~A/attachment-v3/~A" docid docpartid))
