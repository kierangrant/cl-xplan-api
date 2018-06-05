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

File: src/macros.lisp
Description: Client and API Macros
|#

(in-package :cl-xplan-api)

;;; INTERNAL API

(defmacro with-xplan-api-json-handlers (&body body)
  `(json-to-clos:with-json-to-clos (,*default-json-mapper*)
     (let ((json:*json-array-type* 'vector))
       ,@body)))

;;; PUBLIC

(defmacro with-xplan-session
    ((session-name &optional (session-spec NIL) (keep-open NIL) &rest session-opt) &body body)
  "Executes body inside a unwind-protect to optionally close-session if keep-open in NIL, creating session if one isn't provided by session-spec using session-opt as options for session. Errors are ignored when automatically deleting session."
  (let ((ko (gensym)))
    `(let ((,ko ,keep-open)
	   (,session-name ,(if session-spec session-spec `(make-instance 'xplan-session ,@session-opt))))
       (unwind-protect
	    (progn ,@body)
	 (when (not ,ko) (ignore-errors (delete-session ,session-name)))))))

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

;; If we are doing GET, OPTIONS or DELETE, we need to convert XPLAN Types to Hash Table early
;; as they will be converted to URI parameters.
;; In other cases, the content is going in the body and will be encoded to JSON
;; Parameters must be specifed as a series of lists of (KEY VALUE)
(defmacro xplan-call ((session resource method &key content content-type inhibit-auth inhibit-json-decode)
		      &rest parameters)
  (setf parameters (loop for (key value) in parameters collect (cons key value)))
  (let ((c-sym (gensym)) (ct-sym (gensym)) (p-sym (gensym)) (m-sym (gensym)))
    `(let ((,c-sym ,content) (,ct-sym ,content-type) (,p-sym ',parameters) (,m-sym ,method))
       (if (and ,c-sym ,p-sym) (error "Cannot specify both content and parameters. If you need parameters in the URL as well as body content, include it in the resource"))
       (if (and (member ,m-sym '(:GET :DELETE :OPTIONS)) ,c-sym)
	   (error "GET, DELETE and OPTIONS requests CANNOT have body content"))
       (if (and ,p-sym (member ,m-sym '(:GET :DELETE :OPTIONS)))
	   (setf ,p-sym
		 (json-to-clos:with-json-to-clos ((#'(lambda () (make-hash-table :test #'equal))
						     ((t #'(lambda (instance key value)
							     (setf (gethash key instance) value))))))
		   (json:decode-json-from-string (json:encode-json-to-string ,p-sym))))
	   (if (not ,c-sym)		; only have parameters...
	       (setf ,c-sym (json:encode-json-to-string ,p-sym)
		     ,ct-sym "application/json")))
       (process-request
	,session
	(make-instance 'xplan-request
		   :resource ,resource
		   :method ,m-sym
		   :content ,c-sym
		   :content-type ,ct-sym
		   :parameters ,p-sym)
	:inhibit-json-decode ,inhibit-json-decode
	:inhibit-auth ,inhibit-auth))))

(defmacro xplan-prepare-bulk ((bulk-request resource method &key name content (content-type "application/json")
					    inhibit-json-decode omit-results-on-success) &rest parameters)
  (setf parameters (loop for (key value) in parameters collect (cons key value)))
  (let ((c-sym (gensym)) (p-sym (gensym)) (ct-sym (gensym)))
    `(let ((,c-sym ,content) (,ct-sym ,content-type) (,p-sym ,parameters))
       (if (not (string= ,ct-sym "application/json")) (error "Currently only JSON is supported in the body of a BATCHED request"))
       (if (and ,c-sym ,p-sym) (error "Cannot have both content and parameters"))
       (if (not ,c-sym) (setf ,c-sym ,p-sym)) ; conversion of types to JSON will be done later
       (%xplan-prepare-bulk ,bulk-request ,resource ,method :name ,name :content ,c-sym :content-type ,ct-sym  :inhibit-json-decode ,inhibit-json-decode :omit-results-on-success ,omit-results-on-success))))
