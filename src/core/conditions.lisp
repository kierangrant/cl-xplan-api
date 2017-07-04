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

File: src/conditions.lisp
Description: XPLAN API Conditions
|#

(in-package :cl-xplan-api/core)

(define-condition xplan-api-error (error)
  ((status-code :reader xplan-api-error-status-code :initarg :status-code)
   (reason-message :reader xplan-api-error-reason-message :initarg :reason-message :initform NIL)
   (request :reader xplan-api-error-request :initarg :request))
  (:report
   (lambda (c st)
     (format st "XPLAN API Error ~D~:[~;: ~:*~A~]"
	     (xplan-api-error-status-code c)
	     (json:encode-json-to-string
	      (xplan-api-error-reason-message c))))))
