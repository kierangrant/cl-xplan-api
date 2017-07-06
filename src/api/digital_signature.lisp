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

File: src/api/digital_signature.lisp
Description: /digital_signature API Functions
|#

(in-package :cl-xplan-api/api)

;;; digital_signature

;; digital_signature - GET /resourceful/digital_signature and GET /resourceful/digital_signature/:digital_signature_id
(define-entrypoint digital_signature :get
  (digital_signature_id)
  ((status :cond (and (not digital_signature_id) status))
   (date_type :cond (and (not digital_signature_id) date_type))
   (start_date :cond (and (not digital_signature_id) start_date))
   (end_date :cond (and (not digital_signature_id) end_date))
   (entityids :cond (and (not digital_signature_id) entityids))
   (entity_is_signed :cond (and (not digital_signature_id) entity_is_signed))
   (page_size :cond (and (not digital_signature_id) page_size))
   (page_sort :cond (and (not digital_signature_id) page_sort))
   (page_bookmark :cond (and (not digital_signature_id) page_bookmark))
   (page_dir :cond (and (not digital_signature_id) page_dir)))
  :resource (format nil "/digital_signature~@[/~A~]" digital_signature_id))

;;; digital_signature/content

;; digital_signature/content - GET /resourceful/digital_signature/:digital_signature_id/content
(define-entrypoint digital_signature/content :get (digital_signature_id) ()
		   :resource (format nil "/digital_signature/~A/content" digital_signature_id))

;;; digital_signature/content_path

;; digital_signature/content_path -  /resourceful/digital_signature/:digital_signature_id/content_path
(define-entrypoint digital_signature/content_path :get (digital_signature_id) ()
		   :resource (format nil "/digital_signature/~A/content_path" digital_signature_id))

;;; digital_signature/notify

;; digital_signature/notify - POST /resourceful/digital_signature/:digital_signature_id/notify?_method=post
(define-entrypoint digital_signature/notify :post (digital_signature_id) ()
		   :resource (format nil "/digital_signature/~A/notify" digital_signature_id))

;;; digital_signature/signatories

;; digital_signature/signatories - GET /resourceful/digital_signature/:digital_signature_id/signatories
(define-entrypoint digital_signature/signatories :get (digital_signature_id) ()
		   :resource (format nil "/digital_signature/~A/signatories" digital_signature_id))

;;; digital_signature/signatories/generate_code

;; digital_signature/signatories/generate_code - POST /resourceful/digital_signature/:digital_signature_id/signatories/:signatory_id/generate_code?_method=post
(define-entrypoint digital_signature/signatories/generate_code :post
  (digital_signature_id signatory_id) ()
  :single-resource (format nil "/digital_signature/~A/signatories/~A/generate_code?_method=post"
			   digital_signature_id signatory_id)
  :single-parms-as-body T
  :bulk-resource (format nil "/digital_signature/~A/signatories/~A/generate_code"
			 digital_signature_id signatory_id))

;;; digital_signature/signatories/sign

;; digital_signature/signatories/sign - POST /resourceful/digital_signature/:digital_signature_id/signatories/:signatory_id/sign?_method=post
(define-entrypoint digital_signature/signatories/sign :post
  (digital_signature_id signatory_id) (signing_method secure_data otp)
  :single-resource (format nil "/digital_signature/~A/signatories/~A/sign?_method=post"
			   digital_signature_id signatory_id)
  :single-parms-as-body T
  :bulk-resource (format nil "/digital_signature/~A/signatories/~A/sign"
			 digital_signature_id signatory_id))
