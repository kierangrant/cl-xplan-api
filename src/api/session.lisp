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

File: src/api/session.lisp
Description: /session API Functions
|#

(in-package :cl-xplan-api/api)

;;; session

;; use delete-session to delete a session

;; session/transaction - We only support with normal non-bulk requests

;; session/transaction - POST /resourceful/session/transaction
(define-entrypoint session/transaction :post () ()
		   :inhibit-bulk T
		   :inhibit-transaction T
		   :documentation "Create a Session Transaction" :resource "/session/transaction")

;; session/transaction - DELETE /resourceful/session/transaction/:transaction_id
(define-entrypoint session/transaction :delete (transaction_id) ()
		   :inhibit-bulk T
		   :inhibit-transaction T
		   :documentation "Rollback the transaction and dispose of if."
		   :resource (format NIL "/session/transaction/~A" transaction_id))

;; session/transaction - POST /resourceful/session/transaction/:transaction_id?_method=commit - We know that we can use method 'COMMIT' when doing bulk - https://insights.iressconnect.com/docs/DOC-7377#jive_content_id_Commit_a_Transaction
(define-entrypoint session/transaction :commit (transaction_id) ()
		   :inhibit-transaction T
		   :documentation "Commit a Transaction."
		   :single-method :post
		   :single-resource (format NIL "/session/transaction/~A" transaction_id)
		   :hidden-single-parameters (("_method" . "commit"))
		   :bulk-method :comiit
		   :bulk-resource (format NIL "/session/transaction/~A" transaction_id))
