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

File: src/api/error_report.lisp
Description: /error_report API functions
|#

(in-package :cl-xplan-api/api)

;; error_report - POST /resourceful/error_report
(define-entrypoint error_report :post
  () (situation ((logfile nil logfile-p) :cond logfile-p :value (if logfile 1 0)))
  :resource "/error_report"
  :documentation "create a new error report, return the id")
