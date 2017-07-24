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

File: src/api/debt_qualifier/gb/browse_product.lisp
Description: /debt_qualifier/gb/browse_product API Functions
|#

(in-package :cl-xplan-api/api)

;; debt_qualifier/gb/browse_product - GET /resourceful/debt_qualifier/gb/browse_product
(define-entrypoint debt_qualifier/gb/browse_product :get
  ()
  (((proposed_loan_offset nil offset-p) :cond offset-p :value (if proposed_loan_offset 1 0))
   ((applicant_ftb nil ftb-p) :cond ftb-p :value (if applicant_ftb 1 0))
   property_purchase_type property_value proposed_loan_amount proposed_loan_deposit proposed_loan_term
   proposed_loan_type proposed_loan_repayment_type proposed_loan_init_rate_term
   proposed_loan_remortgage_reason proposed_loan_io_amount lender_group rate_type num_scheme
   sort_field sort_order skip_count)
  :resource "/debt_qualifier/gb/browse_product")
