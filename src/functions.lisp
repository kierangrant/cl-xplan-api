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

File: src/functions.lisp
Description: Non-object orientated functions or internal functions
|#

(in-package :cl-xplan-api)

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
	     (json:encode-object-member "value" (xplan-bigdecimal (xplan-value object)))))
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
