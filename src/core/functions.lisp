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

File: src/core/functions.lisp
Description: Non-object orientated functions or internal functions
|#

(in-package :cl-xplan-api/core)

;;; INTERNAL API

(defparameter *default-json-mapper*
  '(#'(lambda () (make-hash-table :test #'equal))
    ((t #'(lambda (instance key value) (setf (gethash key instance) value))
      :mapping
      (#'(lambda () (make-hash-table :test #'equal))
	 ((t #'(lambda (instance key value) (setf (gethash key instance) value)))))))
    :finally #'(lambda (value) (xplan-types->native value))))

;; These functions are helpers to create the types
;; all calls to API functions should pass the appropiate types created by these functions

(defun xplan-date (value)
  (make-instance 'xplan-date :iso8601-str
   (etypecase value
     (string value)
     (integer (rw-ut:write-time-string value "YYYY-MM-DDThh:mm:ssZ")))))
(defmethod xplan-time (value)
  (make-instance
   'xplan-time
   :iso8601-str
   (etypecase value
     (string value)
     (integer (rw-ut:write-time-string value "hh:mm:ssZ")))))
(defun xplan-binary (value)
  "Pass either an Array of (unsigned-byte 8) OR the a Base64 encoded value."
  (make-instance
   'xplan-binary
   :value
   (etypecase value
     ((vector (unsigned-byte 8))
      (cl-base64:usb8-array-to-base64-string value))
     (string value))))
(defun xplan-currency (code value) (make-instance 'xplan-currency :code code :value value))
(defun xplan-bigdecimal (value) (make-instance 'xplan-bigdecimal :value value))

#| If string has an 'e' or 'E' in it, assume it is scientific notation, and do some manual calcs
fixup for some responses having scientific notation.
This is allowed in JSON for a direct value, but this is a string!
We want to return a rational or integer, not a FLOAT
NOTE: We only use xplan-bigdecimal for encoding, we ALWAYS decode to native NUMBER type
|#
(defun bigdecimal-string->native (value)
  (let ((svalue (split-sequence:split-sequence #\e value :test #'string-equal)))
    (if (cdr svalue)
	(* (decimals:parse-decimal-number (car svalue))
	   (expt 10 (decimals:parse-decimal-number (cadr svalue))))
	(decimals:parse-decimal-number value))))

(defun xplan-type->native (value)
  (let ((raw-type (gethash "_type" value))
	(raw-value (gethash "_val" value)))
    (if (not (typep raw-value '(or string hash-table)))
	(error 'type-error :expected-type '(or string hash-table) :datum raw-value))
    (cond
      ((string= raw-type "Date") (xplan-date raw-value))
      ((string= raw-type "Time") (xplan-time raw-value))
      ((string= raw-type "BigDecimal") (bigdecimal-string->native raw-value))
      ((string= raw-type "Currency")
       (xplan-currency (gethash "code" raw-value)
		       (bigdecimal-string->native (gethash "_val" (gethash "value" raw-value)))))
      ((string= raw-type "Binary") (xplan-binary raw-value))
      (T (error 'type-error :expected-type '(OR "Date" "Time" "BigDecimal" "Currency" "Binary")
		:datum raw-type)))))

;; JSON Encoder Method, converts 'native' types to JSON
(defmethod json:encode-json ((object xplan-type) &optional stream)
  (let ((json:*json-output* stream))
    (json:with-object ()
      (json:encode-object-member
       "_type"
       (etypecase object
	 (xplan-date "Date")
	 (xplan-time "Time")
	 (xplan-currency "Currency")
	 (xplan-binary "Binary")
	 (xplan-bigdecimal "BigDecimal")))
      (json:as-object-member ("_val")
	(etypecase object
	  (xplan-iso8601 (json:encode-json (xplan-iso8601-str object)))
	  (xplan-currency
	   (json:with-object ()
	     (json:encode-object-member "code" (xplan-currency-code object))
	     (json:encode-object-member "value" (xplan-value object))))
	  (xplan-binary (json:encode-json (xplan-value object)))
	  (xplan-bigdecimal
	   (json:encode-json
	    (decimals:format-decimal-number (xplan-value object) :round-magnitude *max-rounding*)))))))
  nil)

(defgeneric xplan-types->native (value) (:documentation "Covert all known types in value to native type."))
(defmethod xplan-types->native ((value (eql nil))) nil)
(defmethod xplan-types->native ((value list))
  (handler-case
      (xplan-type->native value)
    (type-error ()
      (cons (xplan-types->native (car value))
	    (xplan-types->native (cdr value))))))
(defmethod xplan-types->native ((value hash-table))
  ;; First try and convert hash table as if it is a XPLAN type
  (handler-case
      (xplan-type->native value)
    (type-error ()
      ;; otherwise try and convert all items in hash-table
      (maphash (lambda (k v) (setf (gethash k value) (xplan-types->native v))) value)
      value)))
(defmethod xplan-types->native ((value string)) value)
(defmethod xplan-types->native ((value vector))
  (loop with restore-vector = T for item across value collect
       (let ((_i))
	 (handler-case
	     (setf _i (xplan-types->native item))
	   (type-error () (setf _i item)))
	 (if (not (eq item _i)) (setf restore-vector nil))
	 _i)
     into items
     finally
       (if restore-vector value (coerce items 'vector))))
(defmethod xplan-types->native ((value T)) value)
