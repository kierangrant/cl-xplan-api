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

File: src/api/entity/client/debt_qualifier/gb/scenario/product_kfi_entry.lisp
Description: /entity/client/debt_qualifier/gb/scenario/product_kfi_entry API functions
|#

(in-package :cl-xplan-api/api)

;;; Version 1

;; entity/client/debt_qualifier/gb/scenario/product_kfi_entry - GET /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/product_kfi_entry/:product_id
(define-entrypoint entity/client/debt_qualifier/gb/scenario/product_kfi_entry :get
  (entity_id scenario_id product_id) ()
  :resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/product_kfi_entry/~A"
		    entity_id scenario_id product_id))

;; entity/client/debt_qualifier/gb/scenario/product_kfi_entry - PATCH /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/product_kfi_entry/:product_id
(define-entrypoint entity/client/debt_qualifier/gb/scenario/product_kfi_entry :patch
  (entity_id scenario_id product_id)
  (level_of_advice adviser_name intermediary_name intermediary_address intermediary_phone proc_fee payable_to
		   note broker_name broker_fee_payment_timing
		   ;; Time for some abstraction violation, unfortunately this is an array of objects including a
		   ;; Boolean field... we need to handle conversion of nil -> json:+json-false+ here
		   (kfi_fees :value
			     (loop
				for val across (coerce kfi_fees 'vector)
				collecting
				;; object can be represented a number of ways...
				;; use flatten-structure to make hash-table we can readily use
				  (let ((h (cl-xplan-api/core::flatten-structure val)))
				    (maphash (lambda (k v) (if (string= k "add_to_loan")
							       (setf (gethash k h) (if v t json:+json-false+))))
					     h)
				    h)
				into vals
				finally (return (coerce vals 'vector))))
		   ((regulated_mortgage nil regulated_mortgage-p) :cond regulated_mortgage-p
		    :value (if regulated_mortgage 1 0))
		   ((include_proc_fee nil include_proc_fee-p) :cond include_proc_fee-p
		    :value (if include_proc_fee 1 0))
		   ((admin_charges nil admin_charges-p) :cond admin_charges-p :value (if admin_charges 1 0)))
  :single-parms-as-body t
  :documentation "kfi_fees is an array of objects containing the fields field_id, amount and add_to_loan"
  :resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/product_kfi_entry/~A"
		    entity_id scenario_id product_id))

;; entity/client-v2/debt_qualifier/gb/scenario/product_kfi_entry - GET /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/product_kfi_entry/:product_id
(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario/product_kfi_entry :get
  (entity_id scenario_id product_id) ()
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/product_kfi_entry/~A"
		    entity_id scenario_id product_id))

;; entity/client-v2/debt_qualifier/gb/scenario/product_kfi_entry - PATCH /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/product_kfi_entry/:product_id
(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario/product_kfi_entry :patch
  (entity_id scenario_id product_id)
  (level_of_advice adviser_name intermediary_name intermediary_address intermediary_phone proc_fee payable_to
		   note broker_name broker_fee_payment_timing
		   ;; Time for some abstraction violation, unfortunately this is an array of objects including a
		   ;; Boolean field... we need to handle conversion of nil -> json:+json-false+ here
		   (kfi_fees :value
			     (loop
				for val across (coerce kfi_fees 'vector)
				collecting
				;; object can be represented a number of ways...
				;; use flatten-structure to make hash-table we can readily use
				  (let ((h (cl-xplan-api/core::flatten-structure val)))
				    (maphash (lambda (k v) (if (string= k "add_to_loan")
							       (setf (gethash k h) (if v t json:+json-false+))))
					     h)
				    h)
				into vals
				finally (return (coerce vals 'vector))))
		   ((regulated_mortgage nil regulated_mortgage-p) :cond regulated_mortgage-p
		    :value (if regulated_mortgage 1 0))
		   ((include_proc_fee nil include_proc_fee-p) :cond include_proc_fee-p
		    :value (if include_proc_fee 1 0))
		   ((admin_charges nil admin_charges-p) :cond admin_charges-p :value (if admin_charges 1 0)))
  :single-parms-as-body t
  :documentation "kfi_fees is an array of objects containing the fields field_id, amount and add_to_loan"
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/product_kfi_entry/~A"
		    entity_id scenario_id product_id))

;; entity/client-v3/debt_qualifier/gb/scenario/product_kfi_entry - GET /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/product_kfi_entry/:product_id
(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario/product_kfi_entry :get
  (entity_id scenario_id product_id) ()
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/product_kfi_entry/~A"
		    entity_id scenario_id product_id))

;; entity/client-v3/debt_qualifier/gb/scenario/product_kfi_entry - PATCH /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/product_kfi_entry/:product_id
(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario/product_kfi_entry :patch
  (entity_id scenario_id product_id)
  (level_of_advice adviser_name intermediary_name intermediary_address intermediary_phone proc_fee payable_to
		   note broker_name broker_fee_payment_timing
		   ;; Time for some abstraction violation, unfortunately this is an array of objects including a
		   ;; Boolean field... we need to handle conversion of nil -> json:+json-false+ here
		   (kfi_fees :value
			     (loop
				for val across (coerce kfi_fees 'vector)
				collecting
				;; object can be represented a number of ways...
				;; use flatten-structure to make hash-table we can readily use
				  (let ((h (cl-xplan-api/core::flatten-structure val)))
				    (maphash (lambda (k v) (if (string= k "add_to_loan")
							       (setf (gethash k h) (if v t json:+json-false+))))
					     h)
				    h)
				into vals
				finally (return (coerce vals 'vector))))
		   ((regulated_mortgage nil regulated_mortgage-p) :cond regulated_mortgage-p
		    :value (if regulated_mortgage 1 0))
		   ((include_proc_fee nil include_proc_fee-p) :cond include_proc_fee-p
		    :value (if include_proc_fee 1 0))
		   ((admin_charges nil admin_charges-p) :cond admin_charges-p :value (if admin_charges 1 0)))
  :single-parms-as-body t
  :documentation "kfi_fees is an array of objects containing the fields field_id, amount and add_to_loan"
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/product_kfi_entry/~A"
		    entity_id scenario_id product_id))

;;; Version 2

;; entity/client/debt_qualifier/gb/scenario/product_kfi_entry-v2 - GET /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/product_kfi_entry-v2/:product_id
(define-entrypoint entity/client/debt_qualifier/gb/scenario/product_kfi_entry-v2 :get
  (entity_id scenario_id product_id) ()
  :resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/product_kfi_entry-v2/~A"
		    entity_id scenario_id product_id))

;; entity/client/debt_qualifier/gb/scenario/product_kfi_entry-v2 - PATCH /resourceful/entity/client/:entity_id/debt_qualifier/gb/scenario/:scenario_id/product_kfi_entry-v2/:product_id
(define-entrypoint entity/client/debt_qualifier/gb/scenario/product_kfi_entry-v2 :patch
  (entity_id scenario_id product_id)
  (level_of_advice adviser_name intermediary_name intermediary_address intermediary_phone proc_fee payable_to
		   note broker_name broker_fee_payment_timing
		   ((regulated_mortgage nil regulated_mortgage-p) :cond regulated_mortgage-p
		    :value (if regulated_mortgage 1 0))
		   ((include_proc_fee nil include_proc_fee-p) :cond include_proc_fee-p
		    :value (if include_proc_fee 1 0))
		   ((admin_charges nil admin_charges-p) :cond admin_charges-p :value (if admin_charges 1 0))
		   ;; In version 2 kfi_fees gets it's own field with an array of objects (!!!)
		   (kfi_fees
		    :value
		    (loop
		       for val across (coerce kfi_fees 'vector)
		       collecting
		       ;; If we use flatten-structure... we end up flattening sub field of array too!
			 (let ((h (cl-xplan-api/core::flatten-structure val)))
			   (maphash (lambda (k v)
				      ;; this will work on add_to_loan as well as sub_items.XXX.add_to_loan
				      (if (search "add_to_loan" k)
					  (setf (gethash k h) (if v t json:+json-false+)))
				      ;; this is for "sub_items.XXX.refundable"
				      (if (search "refundable" k)
					  (setf (gethash k h) (if v t json:+json-false+))))
				    h)
			   h)
		       into vals
		       finally (return (coerce vals 'vector)))))
  :single-parms-as-body t
  :documentation "kfi_fees is an array of objects containing these fields: field_id, amount, add_to_loan, and sub_items. sub_items is an array of objects containing these fields: amount, add_to_loan, refundable, payable_to, payment_timing and note"
  :resource (format nil "/entity/client/~A/debt_qualifier/gb/scenario/~A/product_kfi_entry-v2/~A"
		    entity_id scenario_id product_id))

;; entity/client-v2/debt_qualifier/gb/scenario/product_kfi_entry-v2 - GET /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/product_kfi_entry-v2/:product_id
(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario/product_kfi_entry-v2 :get
  (entity_id scenario_id product_id) ()
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/product_kfi_entry-v2/~A"
		    entity_id scenario_id product_id))

;; entity/client-v2/debt_qualifier/gb/scenario/product_kfi_entry-v2 - PATCH /resourceful/entity/client-v2/:entity_id/debt_qualifier/gb/scenario/:scenario_id/product_kfi_entry-v2/:product_id
(define-entrypoint entity/client-v2/debt_qualifier/gb/scenario/product_kfi_entry-v2 :patch
  (entity_id scenario_id product_id)
  (level_of_advice adviser_name intermediary_name intermediary_address intermediary_phone proc_fee payable_to
		   note broker_name broker_fee_payment_timing
		   ((regulated_mortgage nil regulated_mortgage-p) :cond regulated_mortgage-p
		    :value (if regulated_mortgage 1 0))
		   ((include_proc_fee nil include_proc_fee-p) :cond include_proc_fee-p
		    :value (if include_proc_fee 1 0))
		   ((admin_charges nil admin_charges-p) :cond admin_charges-p :value (if admin_charges 1 0))
		   ;; In version 2 kfi_fees gets it's own field with an array of objects (!!!)
		   (kfi_fees
		    :value
		    (loop
		       for val across (coerce kfi_fees 'vector)
		       collecting
		       ;; If we use flatten-structure... we end up flattening sub field of array too!
			 (let ((h (cl-xplan-api/core::flatten-structure val)))
			   (maphash (lambda (k v)
				      ;; this will work on add_to_loan as well as sub_items.XXX.add_to_loan
				      (if (search "add_to_loan" k)
					  (setf (gethash k h) (if v t json:+json-false+)))
				      ;; this is for "sub_items.XXX.refundable"
				      (if (search "refundable" k)
					  (setf (gethash k h) (if v t json:+json-false+))))
				    h)
			   h)
		       into vals
		       finally (return (coerce vals 'vector)))))
  :single-parms-as-body t
  :documentation "kfi_fees is an array of objects containing these fields: field_id, amount, add_to_loan, and sub_items. sub_items is an array of objects containing these fields: amount, add_to_loan, refundable, payable_to, payment_timing and note"
  :resource (format nil "/entity/client-v2/~A/debt_qualifier/gb/scenario/~A/product_kfi_entry-v2/~A"
		    entity_id scenario_id product_id))

;; entity/client-v3/debt_qualifier/gb/scenario/product_kfi_entry-v2 - GET /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/product_kfi_entry-v2/:product_id
(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario/product_kfi_entry-v2 :get
  (entity_id scenario_id product_id) ()
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/product_kfi_entry-v2/~A"
		    entity_id scenario_id product_id))

;; entity/client-v3/debt_qualifier/gb/scenario/product_kfi_entry-v2 - PATCH /resourceful/entity/client-v3/:entity_id/debt_qualifier/gb/scenario/:scenario_id/product_kfi_entry-v2/:product_id
(define-entrypoint entity/client-v3/debt_qualifier/gb/scenario/product_kfi_entry-v2 :patch
  (entity_id scenario_id product_id)
  (level_of_advice adviser_name intermediary_name intermediary_address intermediary_phone proc_fee payable_to
		   note broker_name broker_fee_payment_timing
		   ((regulated_mortgage nil regulated_mortgage-p) :cond regulated_mortgage-p
		    :value (if regulated_mortgage 1 0))
		   ((include_proc_fee nil include_proc_fee-p) :cond include_proc_fee-p
		    :value (if include_proc_fee 1 0))
		   ((admin_charges nil admin_charges-p) :cond admin_charges-p :value (if admin_charges 1 0))
		   ;; In version 2 kfi_fees gets it's own field with an array of objects (!!!)
		   (kfi_fees
		    :value
		    (loop
		       for val across (coerce kfi_fees 'vector)
		       collecting
		       ;; If we use flatten-structure... we end up flattening sub field of array too!
			 (let ((h (cl-xplan-api/core::flatten-structure val)))
			   (maphash (lambda (k v)
				      ;; this will work on add_to_loan as well as sub_items.XXX.add_to_loan
				      (if (search "add_to_loan" k)
					  (setf (gethash k h) (if v t json:+json-false+)))
				      ;; this is for "sub_items.XXX.refundable"
				      (if (search "refundable" k)
					  (setf (gethash k h) (if v t json:+json-false+))))
				    h)
			   h)
		       into vals
		       finally (return (coerce vals 'vector)))))
  :single-parms-as-body t
  :documentation "kfi_fees is an array of objects containing these fields: field_id, amount, add_to_loan, and sub_items. sub_items is an array of objects containing these fields: amount, add_to_loan, refundable, payable_to, payment_timing and note"
  :resource (format nil "/entity/client-v3/~A/debt_qualifier/gb/scenario/~A/product_kfi_entry-v2/~A"
		    entity_id scenario_id product_id))
