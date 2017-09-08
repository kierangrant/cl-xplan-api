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

File: src/api/session/password_reset_questions.lisp
Description: /session/password_reset_questions API Functions
|#

(in-package :cl-xplan-api/api)

;; password_reset_questions - GET /resourceful/session/password_reset_questions
(define-entrypoint session/password_reset_questions :get
  ()
  ()
  :resource "/session/password_reset_questions"
  :documentation "Return password reset questions.")

;; session/password_reset_questions - POST /resourceful/session/password_reset_questions
(define-entrypoint session/password_reset_questions :post
  (combos) ()
  :content combos
  :content-type "application/json"
  :resource "/session/password_reset_questions"
  :documentation "Override password reset questions. Or create new questions if not existing. combos is an array of objects containing the fields 'question' and 'answer'")
