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

File: src/api/debt_qualifier/gb/broker_fee.lisp
Description: /debt_qualifier/gb/broker_fee API Functions
|#

(in-package :cl-xplan-api/api)

;; debt_qualifier/gb/broker_fee - GET /resourceful/debt_qualifier/gb/broker_fee and GET /resourceful/debt_qualifier/gb/broker_fee/:broker_fee_id
(define-entrypoint debt_qualifier/gb/broker_fee :get
  (broker_fee_id) ()
  :documentation "If lender_id is given, return the broker fee item for current session user, otherwise return all the broker fee items for current session user."
  :resource (format nil "/debt_qualifier/gb/broker_fee/~@[~a~]" broker_fee_id))

;; debt_qualifier/gb/broker_fee - POST /resourceful/debt_qualifier/gb/broker_fee
(define-entrypoint debt_qualifier/gb/broker_fee :post
  () (fee_amount
      payable_to
      fee_timing
      ((add_to_loan nil loan-p) :cond loan-p :value (if add_to_loan 1 0))
      ((refundable nil refundable-p) :cond refundable-p :value (if refundable 1 0)))
  :documentation "Create a new broker fee item for current session user."
  :resource "/debt_qualifier/gb/broker_fee")

;; debt_qualifier/gb/broker_fee - PATCH /resourceful/debt_qualifier/gb/broker_fee/:broker_fee_id
(define-entrypoint debt_qualifier/gb/broker_fee :patch
  (broker_fee_id)
  (fee_amount
   payable_to
   fee_timing
   ((add_to_loan nil loan-p) :cond loan-p :value (if add_to_loan 1 0))
   ((refundable nil refundable-p) :cond refundable-p :value (if refundable 1 0)))
  :documentation "Given the broker fee item Id and new values, update the the broker fee item for current session user."
  :resource (format nil "/debt_qualifier/gb/broker_fee/~A" broker_fee_id)
  :single-method :post
  :hidden-single-parameters (("_method" . "patch"))
  :single-parms-as-body T)

;; debt_qualifier/gb/broker_fee - DELETE /resourceful/debt_qualifier/gb/broker_fee/:broker_fee_id
(define-entrypoint debt_qualifier/gb/broker_fee :delete
  (broker_fee_id) ()
  :documentation "Given the broker fee item Id, remove the broker fee item for current session user."
  :resource (format nil "/debt_qualifier/gb/broker_fee/~A" broker_fee_id))
