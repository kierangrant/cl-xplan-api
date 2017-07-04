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

File: src/functions.lisp
Description: Non-object orientated functions or internal functions
|#

(in-package :cl-xplan-api/core)

;;; INTERNAL API

;; JSON object handler, used internally to change CL-JSON behaviour

(let (obj o-key)
  (defun object-begin ()
    (push (make-hash-table :test #'equal) obj))
  (defun object-end ()
    (pop obj))
  (defun object-key (key)
    (push key o-key))
  (defun object-value (value)
    (setf (gethash (car o-key) (car obj)) value)
    (pop o-key)))

;; blarg, XPlan supports returning fractions and possible dates/time before 1901-00-00T00:00 :/ cannot convert to Universal Time then, just return a string...
(defun convert-xplan-date-to-native (value) value)
(defun convert-xplan-time-to-native (value) value)
(defun convert-xplan-binary-to-native (value) (cl-base64:base64-string-to-usb8-array value))
;; This one is a bit of a hack, Currency's value will be converted to a hash-table from API, and the "value" field in just a regular BigDecimal
(defun convert-xplan-currency-to-native (value) (convert-bulk-to-native value))

#| If string has an 'e' or 'E' in it, assume it is scientific notation, and do some manual calcs
fixup for some responses having scientific notation.
This is allowed in JSON for a direct value, but this is a string!
We want to return a rational or integer, not a FLOAT |#
(defun convert-xplan-bigdecimal-to-native (value)
  (let ((svalue (split-sequence:split-sequence #\e value :test #'string-equal)))
    (if (cdr svalue)
	(* (decimals:parse-decimal-number (car svalue))
	   (expt 10 (decimals:parse-decimal-number (cadr svalue))))
	(decimals:parse-decimal-number value))))

(defun convert-native-to-xplan-bigdecimal (value)
  (let ((str (decimals:format-decimal-number value :round-magnitude *max-rounding*))
	(h (make-hash-table :size 2 :test #'equal)))
    (setf (gethash "_type" h) "BigDecimal"
	  (gethash "_val"  h) str)
    h))

;; Even though we *currently* do not support decoding a Time/Date to a Universal Time, we will support encoding from one
(defun convert-native-to-xplan-date (value)
  (etypecase value
    (number (setf value (rw-ut:write-time-string value "YYYY-MM-DD")))
    (string nil))
  (let ((h (make-hash-table :size 2 :test #'equal)))
    (setf (gethash "_type" h) "Date"
	  (gethash "_val"  h) value)
    h))

(defun convert-native-to-xplan-time (value)
  (etypecase value
    (number (setf value (rw-ut:write-time-string value "YYYY-MM-DDThh:mm:ssZ")))
    (string nil))
  (let ((h (make-hash-table :size 2 :test #'equal)))
    (setf (gethash "_type" h) "Time"
	  (gethash "_val" h) value)
    h))

;; Expaination: {_type: "Currency", _val: {code:  <ISO4217 currency code>, value:  <Decimal value>}}
;; Currently support value being either a Hash-table with "value" being a native value or a p-list with "value" being native value
(defun convert-native-to-xplan-currency (value)
  (let ((h (make-hash-table :size 2 :test #'equal))
	(val (make-hash-table :size 2 :test #'equal)))
    (if (typep value 'hash-table)
	(setf (gethash "code" val) (gethash "code" value)
	      (gethash "value" val) (convert-native-to-xplan-type (gethash "value" value) :bigdecimal))
	(setf (gethash "code" val) (getf value :code)
	      (gethash "value" val) (convert-native-to-xplan-type (getf value :value) :bigdecimal)))
    (setf (gethash "_type" h) "Currency"
	  (gethash "_val" h) val)
    h))

;; We supported usb8 and strings
(defun convert-native-to-xplan-binary (value)
  (let ((h (make-hash-table :size 2 :test #'equal)))
    (setf (gethash "_type" h) "Binary"
	  (gethash "_val" h)
	  (etypecase value
	    ((vector (unsigned-byte 8))
	     (cl-base64:usb8-array-to-base64-string value))
	    (string
	     (cl-base64:string-to-base64-string value))))
    h))

;;; PUBLIC API

;; Convert XPlan Types to Native

(defun convert-xplan-type-to-native (value)
  (let ((raw-type (gethash "_type" value))
	(raw-value (gethash "_val" value)))
    (if (not (typep raw-value '(or string hash-table)))
	(error 'type-error :expected-type '(or string hash-table) :datum raw-value))
    (cond
      ((string= raw-type "Date") (convert-xplan-date-to-native raw-value))
      ((string= raw-type "Time") (convert-xplan-time-to-native raw-value))
      ((string= raw-type "BigDecimal") (convert-xplan-bigdecimal-to-native raw-value))
      ((string= raw-type "Currency") (convert-xplan-currency-to-native raw-value))
      ((string= raw-type "Binary") (convert-xplan-binary-to-native raw-value))
      (T (error 'type-error :expected-type '(OR "Date" "Time" "BigDecimal" "Currency" "Binary")
		:datum raw-type)))))

;; Convert Natives back to XPlan Type

(defun convert-native-to-xplan-type (value type)
  (ecase type
    (:date (convert-native-to-xplan-date value))
    (:time (convert-native-to-xplan-time value))
    (:bigdecimal (convert-native-to-xplan-bigdecimal value))
    (:currency (convert-native-to-xplan-currency value))
    (:binary (convert-native-to-xplan-binary value))))

(defgeneric convert-bulk-to-native (value) (:documentation "Covert all known types in value to native type."))

(defmethod convert-bulk-to-native ((value (eql nil))) nil)

(defmethod convert-bulk-to-native ((value list))
  (handler-case
      (convert-xplan-type-to-native value)
    (type-error ()
      (cons (convert-bulk-to-native (car value))
	    (convert-bulk-to-native (cdr value))))))

(defmethod convert-bulk-to-native ((value hash-table))
  ;; First try and convert hash table as if it is a XPLAN type
  (handler-case
      (convert-xplan-type-to-native value)
    (type-error ()
      ;; otherwise try and convert all items in hash-table
      (maphash (lambda (k v) (setf (gethash k value) (convert-bulk-to-native v))) value)
      value)))

(defmethod convert-bulk-to-native ((value string)) value)

(defmethod convert-bulk-to-native ((value vector))
  (let ((restore-vector T) result)
    (setf result
	  (map 'vector
	       (lambda (x)
		 (let ((item
			(handler-case
			    (convert-bulk-to-native x)
			  (type-error () x))))
		   (if (not (eq item x))
		       (progn
			 (setf restore-vector NIL)
			 item)
		       x)))
	       value))
    (if restore-vector value result)))

(defmethod convert-bulk-to-native ((value T)) value)
