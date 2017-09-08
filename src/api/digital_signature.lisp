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

;; digital_signature - PATCH /resourceful/digital_signature/:digital_signature_id
(define-entrypoint digital_signature :patch
  (digital_signature_id) (due_in_days)
  :resource (format nil "/digital_signature/~A" digital_signature_id))
