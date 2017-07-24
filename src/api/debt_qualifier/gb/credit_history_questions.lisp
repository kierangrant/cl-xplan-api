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

File: src/api/debt_qualifier/gb/credit_history_questions.lisp
Description: /debt_qualifier/gb/credit_history_questions API Functions
|#

(in-package :cl-xplan-api/api)

;;; debt_qualifier/gb/credit_history_questions

;; debt_qualifier/gb/credit_history_questions - GET /resourceful/debt_qualifier/gb/credit_history_questions
(define-entrypoint debt_qualifier/gb/credit_history_questions :get
  () () :resource "/debt_qualifier/gb/credit_history_questions"
  :documentation "Get list of predefined credit history questions")
