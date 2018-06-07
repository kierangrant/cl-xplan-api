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

File: src/functions.lisp
Description: Non-object orientated functions or internal functions
|#

(in-package :cl-xplan-api)

;;; INTERNAL API

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

(defun %xplan-type-finaliser (object)
  (symbol-macrolet ((type (gethash "_type" object)) (value (gethash "_val" object)))
    ;; A Currency Object has a sub-object that is not a Type, but then has a BigDecimal
    ;; So this function will be called for that middle-subtype.
    ;; In this case, we do nothing
    (if (not type) (return-from %xplan-type-finaliser object))
    (pop json-to-clos:current-finally)
    (cond
      ((string= type "Binary") (xplan-binary value))
      ((string= type "Date") (xplan-date value))
      ((string= type "Time") (xplan-time value))
      ((string= type "BigDecimal") (bigdecimal-string->native value))
      ((string= type "Currency") (xplan-currency (gethash "code" value) (gethash "value" value)))
      (t (error "Error decoding XPLAN Type")))))

(defun %xplan-add-value-to-object (instance key value)
  (setf (gethash key instance) value)
  (if (and (string= key "_type")
	   (member value '("Binary" "Date" "Time" "BigDecimal" "Currency") :test #'equal))
      (push #'%xplan-type-finaliser json-to-clos:current-finally))
  value)

(defparameter *default-json-mapper*
  '(#'(lambda () (make-hash-table :test #'equal))
    ((t #'%xplan-add-value-to-object))))

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
	     (json:encode-object-member "value" (xplan-bigdecimal (xplan-value object)))))
	  (xplan-binary (json:encode-json (xplan-value object)))
	  (xplan-bigdecimal
	   (json:encode-json
	    (decimals:format-decimal-number (xplan-value object) :round-magnitude *max-rounding*)))))))
  nil)

#|
Takes an object, and returns a flattened hash-table (test EQUAL).
Keys in returned hash-table are strings.
Keys in passed object must be either a string, integer or a symbol.
Passed object can consist of Strings, Integers, Symbols, Hash-tables, sequences, a-lists, p-list, NIL or T. (CLOS Objects, structures, conditions and functions are *NOT* supported)

Duplicate keys (test is EQUAL on key string) result in last value being used.

A list is treated a p-list if it's first element is a keyword, otherwise it is treated like a sequence. This means you cannot pass a list of keywords and expect it to be treated as a sequence.

Eg:
(flatten-structure '(:foo 100 :bar (:foobar 200)))
--> Hash-table with key, value pairs:
foo: 100
bar.foobar: 200
(flatten-structure '((:foo . 100) ("baR" . "cool") (:test . #(100 200 300))))
--> Hash-table with key, value pairs:
foo: 100
baR: "cool"
test.0: 100
test:1: 200
test:2: 300
(flatten-structure '(:foo NIL :bar T))
--> Hash-table with key, value pairs:
foo: NIL
bar: T
(flatten-structure #(100 200 (:foo "bar")))
--> Hash-table with key, value pairs:
"0": 100
"1": 200
"2.foo": "bar"

Also, be careful with cons cells... you can easily confuse yourself, take for example:
(list (cons ("foo" (list "mee" (list :foo "bar")))))
--> (("foo" "mee" (:FOO "bar")))
(flatten-list (("foo" "mee" (:FOO "bar"))))
--> Hash-table with key, value pairs:
foo.0: "mee"
foo.1.foo: "bar"

This makes sense when you look at the call to list:
1) It makes an a-list, with one item, key "foo"
2) It's value is a list, non-keyword first value, so treated as a sequence
3) Sequence first value is a string
4) Sequence second value is a p-list
|#
(defun flatten-structure (object)
  (let ((pairs (make-hash-table :test #'equal)) (key (make-array 0 :adjustable t :fill-pointer 0)))
    (if (null object) (return-from flatten-structure pairs))
    (labels
	((integer-to-string (int) (with-output-to-string (s) (print-object int s)))
	 (key-to-string (key)
	   (etypecase key
	     (integer (integer-to-string key))
	     (symbol (string-downcase (symbol-name key)))
	     (string key)))
	 (process-object (item)
	   (cond
	     ((or (null item) (eq item T) (typep item 'string) (typep item 'integer)
		  (typep item 'symbol))
	      (process-atom item))
	     ((typep item 'hash-table)
	      (maphash
	       (lambda (k v)
		 (vector-push-extend (key-to-string k) key)
		 (process-object v)
		 (vector-pop key))
	       item))
	     ((typep item 'cons)
	      (if (not (atom (car item)))
		  ;; are an a-list, as is of form ((ATOM . VALUE) (ATOM . VALUE) ...)
		  (loop for (k . v) in item do
		       (vector-push-extend (key-to-string k) key)
		       (process-object v)
		       (vector-pop key))
		  ;; need to detect difference between plist and non p-list, use first element as test
		  (if (symbolp (car item))
		      ;; are a p-list, as is of form (ATOM VALUE ATOM VALUE ...)
		      (loop until (= (length item) 0) do
			   (vector-push-extend (key-to-string (pop item)) key)
			   (process-object (pop item))
			   (vector-pop key))
		      ;; otherwise treat as a regular sequence
		      (dotimes (i (length item))
			(vector-push-extend (integer-to-string i) key)
			(process-object (elt item i))
			(vector-pop key)))))
	     ((typep item 'sequence)
	      (dotimes (i (length item))
		(vector-push-extend (integer-to-string i) key)
		(process-object (elt item i))
		(vector-pop key)))
	     (T (error 'type-error
		       :expected-type '(or string integer symbol vector cons hash-table null
					(member T))
		       :datum item))))
	 (process-atom (item)
	   (setf
	    (gethash
	     (with-output-to-string (s)
	       (dotimes (i (1- (length key)))
		 (write-string (elt key i) s)
		 (write-char #\. s))
	       (write-string (elt key (1- (length key))) s))
	     pairs)
	    item)))
      (process-object object))
    pairs))

(defun %xplan-prepare-bulk (bulk-request resource method
			    &key name content content-type omit-results-on-success inhibit-json-decode)
  (with-slots (requests) bulk-request
    (vector-push-extend
     (make-instance 'xplan-request-bulk
		    :resource resource
		    :method method
		    :content content
		    :content-type content-type
		    :name name
		    :omit-results-on omit-results-on-success
		    :inhibit-json-decode-default inhibit-json-decode)
     requests)))
