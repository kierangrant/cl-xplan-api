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

File: src/api/portfolio/profit_analysis/detail.lisp
Description: /portfolio/profit_analysis/detail API Functions
|#

(in-package :cl-xplan-api/api)

;; portfolio/profit_analysis/detail - GET /resourceful/portfolio/profit_analysis/detail

(define-entrypoint portfolio/profit_analysis/detail :get
  ()
  (start_date end_date grouping from_pricing_type to_pricing_type fields portfolioid accountid
   fees_and_taxes
   ((include_irr nil irr-p) :cond irr-p :value (if include_irr 1 0))
   ((include_twrr nil twrr-p) :cond twrr-p :value (if include_twrr 1 0))
   ((proposed nil proposed-p) :cond proposed-p :value (if proposed 1 0))
   ((exclude_cash nil cash-p) :cond cash-p :value (if exclude_cash 1 0))
   ((apply_excess_unless_fum nil excess-p) :cond excess-p :value (if apply_excess_unless_fum 1 0)
    :string "apply_excess_unless_FUM")
   ((show_hidden_positions nil hidden-p) :cond hidden-p :value (if show_hidden_positions 1 0))
   ((settled nil settled-p) :cond settled-p :value (if settled 1 0)))
  :documentation "Profit analysis report."
  :resource "/portfolio/profit_analysis/detail")
