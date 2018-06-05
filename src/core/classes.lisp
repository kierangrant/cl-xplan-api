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

File: src/core/classes.lisp
Description: XPLAN API Classes
|#

(in-package :cl-xplan-api/core)

(defclass xplan-session ()
  ((api-key :accessor api-key :initarg :api-key)
   (username :accessor username :initarg :username)
   (password :accessor password :initarg :password)
   (base-url :accessor base-url :initarg :base-url)
   (transport-version :accessor transport-version :initarg :transport-version :initform 1)
   (auto-reauth :accessor xplan-session-auto-reauth :initarg :auto-reauth :initform T)
   (force-init-auth :accessor force-init-auth :initarg :force-init-auth :initform NIL)))

(defclass xplan-request ()
  ((response :accessor response :initform NIL)
   (response-msg :accessor response-msg :initform NIL)
   (response-code :accessor response-code :initform 0)
   (response-headers :accessor response-headers :initform NIL)
   (resource :accessor resource :initarg :resource)
   (method :accessor request-method :initarg :method :initform :get)
   (parameters :accessor parameters :initarg :parameters :initform (make-hash-table :test #'string=))
   (content :accessor content :initarg :content :initform NIL)
   (content-type :accessor content-type :initarg :content-type :initform NIL)))

(defclass xplan-request-bulk (xplan-request)
  ((name :accessor name :initarg :name)
   (omit-results-on-success :accessor omit-results-on-success :initform NIL :initarg :omit-results-on)
   (time :accessor response-time :initform NIL)))

;; requests is array of actual requests, the requests themselves store the response
;; state is one of :prepare :processing :done
(defclass xplan-bulk-requests ()
  ((response-code :initform 0)
   (requests :accessor requests :initarg :requests
	     :initform (make-array 0 :element-type 'xplan-request-bulk :adjustable T :fill-pointer 0))
   (include-subtimings :accessor include-subtimings :initarg :include-subtimings :initform NIL)))

(defclass xplan-type () ((value :accessor xplan-value :initarg :value :initform nil)))

(defclass xplan-iso8601 (xplan-type)
  ((value :accessor xplan-iso8601-str :initarg :iso8601-str :documentation "The Date with optional time in ISO 8601 format as a string. In the future methods for accessing the year, month, day, week, etc will be added, for now, you have to parse the string manually")))
(defmethod xplan-value ((instance xplan-iso8601)) (error "Currently cannot convert XPLAN-ISO8601 Types (XPLAN-DATE or XPLAN-TIME) to a native value, please use XPLAN-ISO8601-STR to return a string representation"))

(defclass xplan-date (xplan-iso8601) ())
(defclass xplan-time (xplan-iso8601) ())
(defclass xplan-currency (xplan-type)
  ((code :accessor xplan-currency-code :initarg :code)))
(defclass xplan-binary (xplan-type) ())
(defclass xplan-bigdecimal (xplan-type) ())
