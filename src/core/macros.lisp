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

File: src/macros.lisp
Description: Client and API Macros
|#

(in-package :cl-xplan-api/core)

;;; INTERNAL API

;; Define an API entrypoint.

(defmacro define-entrypoint (name method (&rest extra-parms) (&rest arglist) &key documentation resource inhibit-bulk inhibit-single inhibit-transaction (single-method method) (single-resource resource) (bulk-method method) (bulk-resource resource))
  "Defines an API entry point.

extra-parms are extra keyword parameters for use in expressions for different parameters, used for parameters that MUST NOT come out in API parameters call.

arglist is of form ({field|(field &key field-string cond-expr value-expr)}*):

field is as per lambda list as a keyword argument. Thus instead of a symbol it could be a list, in which case (car field) will be taken as field for the defaults for following.
field-string, if given will provide the string to use in the field, defaults to (STRING-DOWNCASE (SYMBOL-NAME field))
cond-expr is expression used to determine if field will be included, defaults to field
value-expr is expression to use for value of field, defaults to field

Examples of an arglist entry:
field -> (if field `((\"field\" . ,field)))
(field) -> (if field `((\"field\" . ,field)))
((field nil field-p)) -> (if field `((\"field\" . ,field))) ; will not automatically use field-p
(field :field-string \"Field\" :value-expr (symbol-name field)) -> (if field `((\"Field\" . ,(symbol-name field))))
((field \"Something\" field-p) :field-string \"Field_Thing\" :cond-expr (and field-p (not (string= field \"Blarg\"))) :value-expr (somefunc field)) -> (if (and field-p (not (string= field \"Blarg\"))) `((\"Field_Thing\" . ,(somefunc field))))"
(let ((field-entries (loop for item in arglist collecting
			  (if (typep item 'symbol)
			      item
			      (car item)))))
  `(progn
     ,@(if (not (fboundp name))
	   `((defgeneric ,name (session method &key &allow-other-keys))))
     ,@(if (not inhibit-single)
	   `((defmethod ,name ((session xplan-session) (method (eql ,method))
			       &key ,@(if (not inhibit-transaction) '(request-transaction))
				 inhibit-auth inhibit-json-decode return-request
				 ,@extra-parms ,@field-entries &allow-other-keys)
	       ,@(if documentation `(,documentation))
	       (let ((res
		      (xplan-api-call
		       session
		       :inhibit-auth inhibit-auth
		       :inhibit-json-decode inhibit-json-decode
		       :method ,single-method
		       :resource ,single-resource
		       :parameters
		       (append
			,@(if (not inhibit-transaction) `((if request-transaction (list (cons "_transaction" request-transaction)))))
			,@(loop for item in arglist collecting
			       (if (typep item 'symbol)
				   `(if ,item (list (cons ,(string-downcase (symbol-name item)) ,item)))
				   (destructuring-bind (field &key (string nil string-p) (cond field cond-p) (value field value-p))
				       item
				     ;; fixup defaults
				     (if (typep field 'symbol)
					 (setf string (if string-p
							  string
							  (string-downcase (symbol-name field))))
					 (setf string (if string-p string
							  (string-downcase (symbol-name (car field))))
					       cond (if cond-p cond (car field))
					       value (if value-p value (car field))))
				     `(if ,cond (list (cons ,string ,value))))))))))
		 (if return-request
		     res
		     (convert-bulk-to-native (response res)))))))
     ,@(if (not inhibit-bulk)
	   `((defmethod ,name ((session xplan-request-bulk) (method (eql ,method))
			       &key request-name
				 ,@(if (not inhibit-transaction) '(request-transaction))
				 ,@extra-parms ,@field-entries &allow-other-keys)
	       ,@(if documentation `(,documentation))
	       (prepare-request
		session
		:name request-name
		:method ,bulk-method
		:resource ,bulk-resource
		:parameters
		(append
		 ,@(if (not inhibit-transaction) `((if request-transaction (list (cons "_transaction" request-transaction)))))
		 ,@(loop for item in arglist collecting
			(if (typep item 'symbol)
			    `(if ,item (list (cons ,(string-downcase (symbol-name item)) ,item)))
			    (destructuring-bind (field &key (string nil string-p) (cond field cond-p) (value field value-p))
				item
			      ;; fixup defaults
			      (if (typep field 'symbol)
				  (setf string (if string-p
						   string
						   (string-downcase (symbol-name field))))
				  (setf string (if string-p string
						   (string-downcase (symbol-name (car field))))
					cond (if cond-p cond (car field))
					value (if value-p value (car field))))
			      `(if ,cond (list (cons ,string ,value))))))))))))))

(defmacro with-xplan-api-json-handlers (&body body)
  `(let ((json:*beginning-of-object-handler* #'object-begin)
	 (json:*end-of-object-handler* #'object-end)
	 (json:*object-key-handler* #'object-key)
	 (json:*object-value-handler* #'object-value)
	 (json:*json-array-type* 'vector))
     ,@body))


;;; PUBLIC

(defmacro with-xplan-session
    ((session-name &optional (session-spec NIL) (keep-open NIL) &rest session-opt) &body body)
  "Executes body inside a unwind-protect to optionally close-session if keep-open in NIL, creating session if one isn't provided by session-spec using session-opt as options for session. Errors are ignored when automatically deleting session."
  (let ((ko (gensym)))
    `(let ((,ko ,keep-open)
	   (,session-name ,(if session-spec session-spec `(make-instance 'xplan-session ,@session-opt))))
       (unwind-protect
	    (progn ,@body)
	 (when (not ,ko) (ignore-errors (session ,session-name :delete)))))))

(defmacro with-bulk-request ((request-name session &optional (request-spec NIL) &rest request-opt) &body body)
  "Executes body in a let environment where a BULK request is created to request-name if not specified by request-spec using request-opt as options."
  `(let ((,request-name
	  ,(if request-spec
	       request-spec
	       `(make-instance
		 'xplan-request-bulk
		 :session ,session
		 ,@request-opt))))
     ,@body))
