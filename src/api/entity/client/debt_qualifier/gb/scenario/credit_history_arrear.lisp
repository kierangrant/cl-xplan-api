#|
This file is part of CL-XPLAN-API, the Lisp XPLAN API Library

Copyright (C) 2018 Kieran Grant
This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser General Public License
(http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Lisp Lesser GNU General Public License for more details.

File: src/api/entity/client/debt_qualifier/gb/scenario/credit_history_arrear.lisp
Description: /entity/client/debt_qualifier/gb/scenario/credit_history_arrear API functions
|#

(in-package :cl-xplan-api/api)

;;; client

;; entity/client/debt_qualifier/gb/scenario/credit_history_arrear - GET /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id and GET /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id/:answer_item_id
(define-entrypoint entity/client/debt_qualifier/gb/scenario/credit_history_arrear :get
  (entity_id scenario_id answer_id answer_item_id) ()
  :documentation "Answer items are depending on the Credit History Answer. One answer can have 0 or many answer items. The system generated answer id is to be provided to get the collection of answer items. Answer item should only be available if the value of parent Answer.answer is true.

If answer_item_id is present:
Answer item is depending on credit history answer, answer id and answer item id are to be provided together."
  :resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A~@[/~A~]"
		    entity_id scenario_id answer_id answer_item_id))

;; entity/client/debt_qualifier/gb/scenario/credit_history_arrear - POST /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id
(define-entrypoint entity/client/debt_qualifier/gb/scenario/credit_history_arrear :post
  (entity_id scenario_id answer_id) (owner payment_missed date_of_arrear payment_in_arrear amount date_clearance)
  :single-parms-as-body t
  :resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A"
		    entity_id scenario_id answer_id))

;; entity/client/debt_qualifier/gb/scenario/credit_history_arrear - POST /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id?_method=abstractcreate
(define-entrypoint entity/client/debt_qualifier/gb/scenario/credit_history_arrear :abstractcreate
  (entity_id scenario_id answer_id) ()
  :single-method :post
  :single-resource
  (format nil
	  "/entity/client/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A?_method=abstractcreate"
	  entity_id scenario_id answer_id)
  :resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A" entity_id
		    scenario_id answer_id))

;; entity/client/debt_qualifier/gb/scenario/credit_history_arrear - POST /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id?_method=abstractread and POST /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id/:answer_item_id?_method=abstractread
(define-entrypoint entity/client/debt_qualifier/gb/scenario/credit_history_arrear :abstractread
  (entity_id scenario_id answer_id answer_item_id) ()
  :single-method :post
  :single-resource
  (format nil
	  "/entity/client/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A~@[/~A~]?_method=abstractread"
	  entity_id scenario_id answer_id answer_item_id)
  :resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A~@[/~A~]" entity_id
		    scenario_id answer_id answer_item_id))

;; entity/client/debt_qualifier/gb/scenario/credit_history_arrear - PATCH /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id/:answer_item_id
(define-entrypoint entity/client/debt_qualifier/gb/scenario/credit_history_arrear :patch
  (entity_id scenario_id answer_id answer_item_id)
  (owner payment_missed date_of_arrear payment_in_arrear amount date_clearance)
  :single-parms-as-body t
  :resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A/~A"
		    entity_id scenario_id answer_id answer_item_id))

;; entity/client/debt_qualifier/gb/scenario/credit_history_arrear - DELETE /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id/:answer_item_id
(define-entrypoint entity/client/debt_qualifier/gb/scenario/credit_history_arrear :delete
  (entity_id scenario_id answer_id answer_item_id) ()
  :resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A/~A"
		    entity_id scenario_id answer_id answer_item_id))

;; entity/client/debt_qualifier/gb/scenario/credit_history_arrear - POST /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id/:answer_item_id?_method=abstractupdate
(define-entrypoint entity/client/debt_qualifier/gb/scenario/credit_history_arrear :abstractupdate
  (entity_id scenario_id answer_id answer_item_id) ()
  :resource
  (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A/~A?_method=abstractupdate"
	  entity_id scenario_id answer_id answer_item_id))

;;; client-v2

;; entity/client-v2/debt_qualifier/gb/scenario/credit_history_arrear - GET /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id and GET /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id/:answer_item_id
(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario/credit_history_arrear :get
  (entity_id scenario_id answer_id answer_item_id) ()
  :documentation "Answer items are depending on the Credit History Answer. One answer can have 0 or many answer items. The system generated answer id is to be provided to get the collection of answer items. Answer item should only be available if the value of parent Answer.answer is true.

If answer_item_id is present:
Answer item is depending on credit history answer, answer id and answer item id are to be provided together."
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A~@[/~A~]"
		    entity_id scenario_id answer_id answer_item_id))

;; entity/client-v2/debt_qualifier/gb/scenario/credit_history_arrear - POST /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id
(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario/credit_history_arrear :post
  (entity_id scenario_id answer_id) (owner payment_missed date_of_arrear payment_in_arrear amount date_clearance)
  :single-parms-as-body t
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A"
		    entity_id scenario_id answer_id))

;; entity/client-v2/debt_qualifier/gb/scenario/credit_history_arrear - POST /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id?_method=abstractcreate
(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario/credit_history_arrear :abstractcreate
  (entity_id scenario_id answer_id) ()
  :single-method :post
  :single-resource
  (format nil
	  "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A?_method=abstractcreate"
	  entity_id scenario_id answer_id)
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A" entity_id
		    scenario_id answer_id))

;; entity/client-v2/debt_qualifier/gb/scenario/credit_history_arrear - POST /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id?_method=abstractread and POST /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id/:answer_item_id?_method=abstractread
(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario/credit_history_arrear :abstractread
  (entity_id scenario_id answer_id answer_item_id) ()
  :single-method :post
  :single-resource
  (format nil
	  "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A~@[/~A~]?_method=abstractread"
	  entity_id scenario_id answer_id answer_item_id)
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A~@[/~A~]"
		    entity_id scenario_id answer_id answer_item_id))

;; entity/client-v2/debt_qualifier/gb/scenario/credit_history_arrear - PATCH /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id/:answer_item_id
(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario/credit_history_arrear :patch
  (entity_id scenario_id answer_id answer_item_id)
  (owner payment_missed date_of_arrear payment_in_arrear amount date_clearance)
  :single-parms-as-body t
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A/~A"
		    entity_id scenario_id answer_id answer_item_id))

;; entity/client-v2/debt_qualifier/gb/scenario/credit_history_arrear - DELETE /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id/:answer_item_id
(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario/credit_history_arrear :delete
  (entity_id scenario_id answer_id answer_item_id) ()
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A/~A"
		    entity_id scenario_id answer_id answer_item_id))

;; entity/client-v2/debt_qualifier/gb/scenario/credit_history_arrear - POST /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id/:answer_item_id?_method=abstractupdate
(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario/credit_history_arrear :abstractupdate
  (entity_id scenario_id answer_id answer_item_id) ()
  :resource
  (format nil
	  "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A/~A?_method=abstractupdate"
	  entity_id scenario_id answer_id answer_item_id))

;;; client-v3

;; entity/client-v3/debt_qualifier/gb/scenario/credit_history_arrear - GET /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id and GET /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id/:answer_item_id
(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario/credit_history_arrear :get
  (entity_id scenario_id answer_id answer_item_id) ()
  :documentation "Answer items are depending on the Credit History Answer. One answer can have 0 or many answer items. The system generated answer id is to be provided to get the collection of answer items. Answer item should only be available if the value of parent Answer.answer is true.

If answer_item_id is present:
Answer item is depending on credit history answer, answer id and answer item id are to be provided together."
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A~@[/~A~]"
		    entity_id scenario_id answer_id answer_item_id))

;; entity/client-v3/debt_qualifier/gb/scenario/credit_history_arrear - POST /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id
(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario/credit_history_arrear :post
  (entity_id scenario_id answer_id) (owner payment_missed date_of_arrear payment_in_arrear amount date_clearance)
  :single-parms-as-body t
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A"
		    entity_id scenario_id answer_id))

;; entity/client-v3/debt_qualifier/gb/scenario/credit_history_arrear - POST /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id?_method=abstractcreate
(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario/credit_history_arrear :abstractcreate
  (entity_id scenario_id answer_id) ()
  :single-method :post
  :single-resource
  (format nil
	  "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A?_method=abstractcreate"
	  entity_id scenario_id answer_id)
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A" entity_id
		    scenario_id answer_id))

;; entity/client-v3/debt_qualifier/gb/scenario/credit_history_arrear - POST /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id?_method=abstractread and POST /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id/:answer_item_id?_method=abstractread
(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario/credit_history_arrear :abstractread
  (entity_id scenario_id answer_id answer_item_id) ()
  :single-method :post
  :single-resource
  (format nil
	  "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A~@[/~A~]?_method=abstractread"
	  entity_id scenario_id answer_id answer_item_id)
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A~@[/~A~]"
		    entity_id scenario_id answer_id answer_item_id))

;; entity/client-v3/debt_qualifier/gb/scenario/credit_history_arrear - PATCH /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id/:answer_item_id
(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario/credit_history_arrear :patch
  (entity_id scenario_id answer_id answer_item_id)
  (owner payment_missed date_of_arrear payment_in_arrear amount date_clearance)
  :single-parms-as-body t
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A/~A"
		    entity_id scenario_id answer_id answer_item_id))

;; entity/client-v3/debt_qualifier/gb/scenario/credit_history_arrear - DELETE /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id/:answer_item_id
(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario/credit_history_arrear :delete
  (entity_id scenario_id answer_id answer_item_id) ()
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A/~A"
		    entity_id scenario_id answer_id answer_item_id))

;; entity/client-v3/debt_qualifier/gb/scenario/credit_history_arrear - POST /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/credit_history_arrear/:answer_id/:answer_item_id?_method=abstractupdate
(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario/credit_history_arrear :abstractupdate
  (entity_id scenario_id answer_id answer_item_id) ()
  :resource
  (format nil
	  "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/credit_history_arrear/~A/~A?_method=abstractupdate"
	  entity_id scenario_id answer_id answer_item_id))
