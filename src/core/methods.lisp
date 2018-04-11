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

File: src/core/methods.lisp
Description: Methods for Classes
|#

(in-package :cl-xplan-api/core)

;;; INTERNAL

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

(defgeneric get-request-url (request))

(defmethod get-request-url ((request xplan-request-bulk))
  (with-slots (session) request
    (with-slots (base-url transport-version) session
      (format NIL "~A/resourceful-v~D" base-url transport-version))))

(defmethod get-request-url ((request xplan-request))
  (with-slots (resource parameters session) request
    (with-slots (base-url transport-version) session
      (format nil "~a/resourceful-v~D~a~@[~a~]" base-url transport-version resource
	      (with-output-to-string (out)
		(labels ((value-to-string (item)
			   (etypecase item
			     (string (drakma:url-encode item :utf-8))
			     (integer
			      (drakma:url-encode
			       (decimals:format-decimal-number item :round-magnitude -20)  :utf-8))
			     (symbol (drakma:url-encode (string-downcase (symbol-name item))  :utf-8))
			     ((member nil) nil))))
		  (with-hash-table-iterator (getitem (flatten-structure parameters))
		    (let ((result (multiple-value-list (getitem))))
		      (if (car result)
			  ;; don't URL encode transaction ID or bookmark, XPLAN throws errors
			  (if (or (string= (elt result 1) "_transaction")
				  (string= (elt result 1) "page_bookmark"))
			      (format out "?~a~@[=~a~]" (elt result 1) (elt result 2))
			      (format out "?~a~@[=~a~]" (elt result 1)
				      (value-to-string (elt result 2)))))
		      (loop
			 for result = (multiple-value-list (getitem))
			 while (car result)
			 do
			 ;; don't URL encode transaction ID or bookmark, XPLAN throws errors
			   (if (or (string= (elt result 1) "_transaction")
				   (string= (elt result 1) "page_bookmark"))
			       (format out "&~a~@[=~a~]" (elt result 1) (elt result 2))
			       (format out "&~a~@[=~a~]" (elt result 1)
				       (value-to-string (elt result 2)))))))))))))

(defgeneric get-request-content (request))

(defmethod get-request-content ((request xplan-request-bulk))
  (let* ((request-call-obj
	  `((:batch
	     .
	     ,(coerce
	       (loop for item across (requests request) collecting
		    (let ((result
			   `((:method . ,(if (symbolp #0=(request-method item))(symbol-name #0#) #0#))
			     (:url . ,(resource item))
			     (:omit--results--on--success
			      .
			      ,(if (omit-results-on-success item) T json:+json-false+)))))
		      (if (parameters item) (setf result (acons :body (parameters item) result)))
		      (if (name item) (setf result (acons :name (name item) result)))
		      result))
	       'vector))
	    (:include_subtimings . ,(if (include-subtimings request) T json:+json-false+))))
	 (request-call (json:encode-json-to-string request-call-obj)))
    request-call))

(defmethod get-request-content ((request xplan-request)) (content request))

(defgeneric get-request-content-type (request))

(defmethod get-request-content-type ((request xplan-request-bulk)) "application/json")

(defmethod get-request-content-type ((request xplan-request)) (content-type request))

(defgeneric get-request-method (request))

(defmethod get-request-method ((request xplan-request-bulk)) :post)

(defmethod get-request-method ((request xplan-request)) (request-method request))

(defgeneric %process-request (session request &key do-auth))

(defmethod %process-request ((session xplan-session) request &key do-auth)
  (with-slots (session-state api-key username password drakma-settings force-init-auth base-url)
      session
    (let (response
	  (content (get-request-content request))
	  (content-type (get-request-content-type request))
	  (request-url (get-request-url request))
	  (method (get-request-method request)))
      (if (or do-auth force-init-auth)
	  (progn
	    ;; disable force-init-auth on first chance
	    (setf force-init-auth NIL
		  session-state (make-instance 'drakma:cookie-jar))
	    (setf response
		  (multiple-value-list
		   (apply
		    *api-call-function*
		    request-url
		    :method method
		    :force-binary T
		    :cookie-jar session-state
		    :user-agent *user-agent*
		    :additional-headers
		    `(("X-Xplan-App-Id" . ,api-key)
		      ("Accept" . "application/json")
		      ("Referer" . ,base-url))
		    :basic-authorization (list username password)
		    (if (and content content-type)
			(append
			 `(:content ,content :content-type ,content-type)
			 drakma-settings)
			drakma-settings)))))
	  (setf response
		(multiple-value-list
		 (apply
		  *api-call-function*
		  request-url
		  :method :post
		  :force-binary T
		  :cookie-jar session-state
		  :user-agent *user-agent*
		  :additional-headers
		  `(("X-Xplan-App-Id" . ,api-key)
		    ("Accept" . "application/json")
		    ("Referer" . ,base-url))
		  (if (and content content-type)
		      (append
		       `(:content ,content :content-type ,content-type)
		       drakma-settings)
		      drakma-settings)))))
      (values-list response))))

;;; PUBLIC

(defgeneric get-request-by-name (request name))
(defmethod get-request-by-name ((request xplan-request-bulk) name)
  (let ((req
	 (remove-if
	  #'null
	  (map
	   'vector
	   (lambda (item) (if (string= name (name item)) item))
	   (requests request)))))
    (if (> (length req) 1)
	(error 'xplan-api-error :request request :reason-message "Upstream Server returned more than 1 request with the same name, don't know how to handle this." :status-code 502))
    (if (= (length req) 1)
	(elt req 0)
	NIL)))

(defgeneric xplan-api-call (xplan-session &key resource method parameters content content-type inhibit-auth inhibit-json-decode &allow-other-keys))

(defmethod xplan-api-call ((xplan-session xplan-session) &key resource (method :get) parameters content content-type inhibit-auth inhibit-json-decode &allow-other-keys)
  (let ((request
	 (make-instance
	  'xplan-request
	  :session xplan-session
	  :resource resource
	  :method method
	  :parameters parameters
	  :content content
	  :content-type content-type)))
    (process-request request :inhibit-auth inhibit-auth :inhibit-json-decode inhibit-json-decode)
    request))

(defgeneric prepare-request (request &key resource method parameters &allow-other-keys))
(defmethod prepare-request ((request xplan-request-bulk) &key resource method parameters name omit-results-on-success inhibit-json-decode)
  (with-slots (state requests) request
    (if (not (eq state :prepare))
	(error 'xplan-api-error :request request
	       :reason-message "Cannot prepare a request once BULK request is processing or is finished"
	       :status-code 400))
    (vector-push-extend
     (make-instance 'xplan-request-bulk-requests
		    :resource (concatenate 'string "/resourceful" resource)
		    :method method
		    :parameters parameters
		    :name name
		    :omit-results-on-success omit-results-on-success
		    :inhibit-json-decode-default inhibit-json-decode)
     requests)))

(defgeneric delete-session (session) (:documentation "Deletes a session, ignores HTTP 401 errors, returns T if session was delted, otherwise NIL. On non 401 Errors a XPLAN-API-ERROR condition is thrown."))
(defmethod delete-session ((session xplan-session))
  (let (error)
    (setf (slot-value session 'force-init-auth) NIL)
    (handler-case
	(xplan-api-call session :resource "/session" :method :delete :inhibit-auth T)
      (xplan-api-error (e)
	;; if an error happens whilst Deleting a session, did it delete or not?!?
	;; to be safe, don't invalidate the session... if it did delete, next use will think
	;; the server invalidated it and re-auth normally..
	;; We ignore HTTP 401 errors
	(setf error T)
	(if (and (>= (xplan-api-error-status-code e) 400)
		 (not (= 401 (xplan-api-error-status-code e))))
	    (error e))))
    (setf (session-state session) (make-instance 'drakma:cookie-jar))
    (not error)))

(defgeneric process-request (request &key inhibit-auth &allow-other-keys)
  (:documentation "Process the request, if inhibit-auth is T, override all other parameters and refuse to reauthenticate on HTTP 401"))

(defmethod process-request ((request xplan-request-bulk)
			    &key inhibit-auth (inhibit-json-decode nil inhibit-json-decode-p)
			      ignore-subrequest-errors)
  (with-slots (state session) request
    (if (not (eq state :prepare))
	(error 'xplan-api-error
	       :status-code 400
	       :request request
	       :reason-message "Cannot process request already being processed or finished processing."))
    (setf state :processing)
    (let (response decoded-response)
      (if inhibit-auth
	  (progn
	    (setf response (multiple-value-list (%process-request session request :do-auth NIL)))
	    (if (>= (elt response 1) 400)
		(error 'xplan-api-error
		       :response-message
		       (if (elt response 0)
			   (with-xplan-api-json-handlers
			     (json:decode-json-from-string (babel:octets-to-string (elt response 0))))
			   (elt response 6))
		       :status-code (elt response 1)
		       :request request)))
	  (let (do-auth)
	    (tagbody
	     restart
	       (setf response (multiple-value-list (%process-request session request :do-auth do-auth)))
	       (if (and (= (elt response 1) 401) (xplan-session-auto-reauth session) (not do-auth))
		   (progn
		     (setf do-auth T)
		     (go restart)))
	       ;; If we failed re-auth, or was disabled, or server error, throw error
	       (if (>= (elt response 1) 400)
		   (error 'xplan-api-error
			  :reason-message
			  (if (elt response 0)
			      (with-xplan-api-json-handlers
				(json:decode-json-from-string
				 (babel:octets-to-string (elt response 0))))
			      (elt response 6))
			  :status-code (elt response 1)
			  :request request)))))
      ;; now we have response, and no error occured, let's process it
      (setf decoded-response (with-xplan-api-json-handlers (json:decode-json-from-string (babel:octets-to-string (elt response 0)))))
      (loop for res across decoded-response do
	   (if (null (get-request-by-name request (gethash "name" res)))
	       (progn
		 (format *xplan-api-debug* "Server returned a request name that I don't have, name: ~a~%" (gethash "name" res))
		 (error 'xplan-api-error :reason-message "Server returned a request name that I don't have" :status-code 500 :request request)))
	   (with-slots (response response-msg response-code response-headers name time
				 inhibit-json-decode-default)
	       (get-request-by-name request (gethash "name" res))
	     (setf response
		   (if (and (gethash "body" res) (not (= (gethash "code" res) 204)))
		       (if (or (and inhibit-json-decode-p inhibit-json-decode)
			       (and (not inhibit-json-decode-p) inhibit-json-decode-default))
			   (gethash "body" res)
			   (with-xplan-api-json-handlers
			     (convert-bulk-to-native
			      (json:decode-json-from-string (gethash "body" res)))))
		       nil)
		   response-msg (gethash "msg" res)
		   response-code (gethash "code" res)
		   response-headers (gethash "headers" res)
		   time (gethash "time" res)))))
    ;; Now we have populated the Bulk Request object with the results, let's see if any subrequests had an error and throw an exception if they did
    (loop for req across (requests request) do
	 (if (and (not ignore-subrequest-errors) (>= (response-code req) 400))
	     (cerror
	      "Continue processing sub-requests"
	      'xplan-api-error
	      :request req
	      :reason-message
	      (if (response req) (json:encode-json-to-string (response req)) (response-msg req))
	      :status-code (response-code req))))
    (setf state :done)))

(defmethod process-request ((request xplan-request) &key inhibit-auth inhibit-json-decode)
  (with-slots (response response-code response-headers state session) request
    (if (not (eq state :prepare))
	(error "Cannot process request already being processed or finished processing."))
    (setf state :processing)
    ;; if inhibit-auth is set, we will skip-authentication
    (let (%response)
      (if inhibit-auth
	  (progn
	    (setf %response (multiple-value-list (%process-request session request :do-auth NIL)))
	    (if (>= (elt %response 1) 400)
		(error 'xplan-api-error
		       :response-message
		       (if (elt %response 0)
			   (with-xplan-api-json-handlers
			     (json:decode-json-from-string (babel:octets-to-string (elt %response 0))))
			   (elt %response 6))
		       :status-code (elt %response 1)
		       :request request)))
	  (let (do-auth)
	    ;; handle re-authentication
	    (tagbody
	     restart
	       (setf %response (multiple-value-list (%process-request session request :do-auth do-auth)))
	       (if (and (= (elt %response 1) 401) (xplan-session-auto-reauth session) (not do-auth))
		   (progn
		     (setf do-auth T)
		     (go restart)))
	       ;; if we failed re-auth, or was disabled, or server error, throw error
	       (if (>= (elt %response 1) 400)
		   (error 'xplan-api-error
			  :reason-message
			  (if (elt %response 0)
			      (with-xplan-api-json-handlers
				(json:decode-json-from-string (babel:octets-to-string (elt %response 0))))
			      (elt %response 6))
			  :status-code (elt %response 1)
			  :request request)))))
      (setf
       response (if (elt %response 0)
		    (if inhibit-json-decode
			(babel:octets-to-string (elt %response 0))
			(with-xplan-api-json-handlers
			  (convert-bulk-to-native
			   (json:decode-json-from-string (babel:octets-to-string (elt %response 0))))))
		    nil)
       response-headers (elt %response 2)
       response-code (elt %response 1)))
    (setf state :done)))
