#| -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-XPLAN-API; Base: 10 -*-

This file is part of CL-XPLAN-API, the Lisp XPLAN API Library
Copyright (C) 2018 Kieran Grant

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

See LICENSE for the full license

File: src/conditions.lisp
Description: XPLAN API Conditions
|#

(in-package :cl-xplan-api)

(define-condition xplan-api-error (error)
  ((request :reader xplan-api-error-request :initarg :request)
   (reason :reader xplan-api-error-reason :initarg :reason)))

(define-condition xplan-api-error-bad-request (xplan-api-error) ())
(define-condition xplan-api-error-unauthenticated (xplan-api-error) ())
(define-condition xplan-api-error-unauthorised (xplan-api-error) ())
(define-condition xplan-api-error-not-found (xplan-api-error) ())
(define-condition xplan-api-error-server-error (xplan-api-error) ())
(define-condition xplan-api-error-upstream-server-error (xplan-api-error-server-error) ())

(defmethod xplan-api-error-code ((c xplan-api-error-bad-request)) (declare (ignore c)) 400)
(defmethod xplan-api-error-code ((c xplan-api-error-unauthenticated)) (declare (ignore c)) 401)
(defmethod xplan-api-error-code ((c xplan-api-error-unauthorised)) (declare (ignore c)) 403)
(defmethod xplan-api-error-code ((c xplan-api-error-not-found)) (declare (ignore c)) 404)
(defmethod xplan-api-error-code ((c xplan-api-error-server-error)) (declare (ignore c)) 500)
(defmethod xplan-api-error-code ((c xplan-api-error-upstream-server-error)) (declare (ignore c)) 502)
