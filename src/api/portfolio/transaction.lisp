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

File: src/api/portfolio/transaction.lisp
Description: /portfolio/transaction API Functions
|#

(in-package :cl-xplan-api/api)

;; portfolio/transaction - GET /resourceful/portfolio/transaction/:transaction_id and GET /resourceful/portfolio/transaction
(define-entrypoint portfolio/transaction :get
  (transaction_id)
  (fields
   (start_date :cond (and (not transaction_id) start_date))
   (stop_date :cond (and (not transaction_id) stop_date))
   (page_bookmark :cond (and (not transaction_id) page_bookmark))
   (page_size :cond (and (not transaction_id) page_size))
   (sort_type :cond (and (not transaction_id) sort_type))
   (date_filter :cond (and (not transaction_id) date_filter))
   (exclude_cash :cond (and (not transaction_id) exclude_cash))
   (transaction_type :cond (and (not transaction_id) transaction_type))
   (transaction_status :cond (and (not transaction_id) transaction_status))
   (security_id :cond (and (not transaction_id) security_id))
   (vendor :cond (and (not transaction_id) vendor))
   (external_id :cond (and (not transaction_id) external_id))
   (external_id_contains :cond (and (not transaction_id) external_id_contains))
   (currency :cond (and (not transaction_id) currency))
   (portfolioid :cond (and (not transaction_id) portfolioid))
   (accountid :cond (and (not transaction_id) accountid))
   ((apply_exclude_unless_fum nil apply-exclude-p) :cond (and (not transaction_id) apply-exclude-p)
    :value (if apply_exclude_unless_fum 1 0) :string "apply_exclude_unless_FUM")
   ((proposed nil proposed-p) :cond proposed-p :value (if proposed 1 0))
   ((show_hidden_positions nil show-hidden-p) :cond show-hidden-p
    :value (if show_hidden_positions 1 0)))
  :documentation "Retrieve a transaction or a collection.
If you provide a transaction ID (as an integer) only the fields paramater is used.
If no portfolio is given, all transactions visible to the user are returned."
  :resource (format nil "/portfolio/transaction~@[/~A~]" transaction_id))

;; portfolio/transaction - PATCH /resourceful/portfolio/transaction/:transaction_id
(define-entrypoint portfolio/transaction :patch
  (transaction_id)
  (transaction_type comment trade_date tax_date settlement_date volume value price_per_unit transaction_status transaction_subtype)
  :single-parms-as-body t
  :resource (format NIL "/portfolio/transaction/~A" transaction_id))

;; portfolio/transaction - DELETE /resourceful/portfolio/transaction/:transaction_id
(define-entrypoint portfolio/transaction :delete
  (transaction_id) ()
  :resource (format nil "/portfolio/transaction/~A" transaction_id))
