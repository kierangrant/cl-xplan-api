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

File: src/methods.lisp
Description: Methods for Classes
|#

(in-package :cl-xplan-api/core)

;;; INTERNAL

(defgeneric request-to-json (request))
(defmethod request-to-json ((request xplan-request-bulk))
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

(defun alist-to-url-encoding (alist)
  "Takes an alist, and converts it to a URL-encoded list, converts sequences to correct form"
  (if (null alist) (return-from alist-to-url-encoding nil))
  (let (strings)
    (loop for (key . value) in alist do
	 (typecase value
	   (string
	    (setf strings (append strings (cons (concatenate 'string key "=" value) nil))))
	   (integer
	    (setf strings
		  (append strings
			  (cons (concatenate 'string key "=" (format NIL "~D" value)) NIL))))
	   (vector
	    (loop
	       for item across value
	       for index from 0 to (1- (length value)) do
		 (setf strings (append strings (cons (format nil "~a.~D=~a" key index item) nil)))))
	   (sequence
	    (loop
	       for item in value
	       for index from 0 to (1- (length value)) do
		 (setf strings (append strings (cons (format nil "~a.~D=~a" key index item) nil)))))
	   (t
	    (setf strings (append strings (cons key nil))))))
    (format nil "~{~a~^&~}" strings)))

(defgeneric %process-request (request &key))

(defmethod %process-request ((request xplan-request-bulk) &key do-auth)
  (with-slots (session) request
    (with-slots (session-state base-url transport-version api-key username password drakma-settings force-init-auth)
	session
      (let (response
	    (content (request-to-json request))
	    (request-url (concatenate 'string base-url (format NIL "/resourceful-v~D" transport-version))))
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
		      :method :post
		      :force-binary T
		      :content content
		      :content-type "application/json"
		      :cookie-jar session-state
		      :user-agent *user-agent*
		      :additional-headers
		      `(("X-Xplan-App-Id" . ,api-key)
			("Accept" . "application/json"))
		      :basic-authorization (list username password)
		      drakma-settings))))
	    (setf response
		  (multiple-value-list
		   (apply
		    *api-call-function*
		    request-url
		    :method :post
		    :force-binary T
		    :content content
		    :content-type "application/json"
		    :cookie-jar session-state
		    :user-agent *user-agent*
		    :additional-headers
		    `(("X-Xplan-App-Id" . ,api-key)
		      ("Accept" . "application/json"))
		    drakma-settings))))
	(values-list response)))))

(defmethod %process-request ((request xplan-request) &key do-auth)
  (with-slots (resource method parameters state session content content-type) request
    (with-slots (session-state base-url transport-version api-key
			       username password drakma-settings force-init-auth)
	session
      (let ((drakma-response)
	    (request-url
	     (format NIL "~a/resourceful-v~D~a~:[~;?~:*~a~]" base-url transport-version resource
		     (alist-to-url-encoding parameters))))
	(if (or do-auth force-init-auth)
	    (progn
	      ;; disable force-init-auth on first chance
	      (setf force-init-auth NIL
		    session-state (make-instance 'drakma:cookie-jar))
	      (setf drakma-response
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
			("Accept" . "application/json"))
		      :basic-authorization (list username password)
		      (if (and content content-type)
			  (append
			   `(:content content :content-type content-type)
			   drakma-settings)
			  drakma-settings)))))
	    (setf drakma-response
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
		      ("Accept" . "application/json"))
		    (if (and content content-type)
			(append
			 `(:content content :content-type content-type)
			 drakma-settings)
			drakma-settings)))))
	(values-list drakma-response)))))

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
(defmethod prepare-request ((request xplan-request-bulk) &key resource method parameters name omit-results-on-success)
  (with-slots (state requests) request
    (if (not (eq state :prepare))
	(error "Cannot prepare a request once BULK request is processing or is finished"))
    (vector-push-extend
     (make-instance 'xplan-request-bulk-requests
		    :resource (concatenate 'string "/resourceful" resource)
		    :method method
		    :parameters parameters
		    :name name
		    :omit-results-on-success omit-results-on-success)
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

(defmethod process-request ((request xplan-request-bulk) &key inhibit-auth inhibit-json-decode)
  (with-slots (state session) request
    (if (not (eq state :prepare))
	(error "Cannot process request already being processed or finished processing."))
    (setf state :processing)
    (let (response decoded-response)
      (if inhibit-auth
	  (progn
	    (setf response (multiple-value-list (%process-request request :do-auth NIL)))
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
	       (setf response (multiple-value-list (%process-request request :do-auth do-auth)))
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
	   (with-slots (response response-msg response-code response-headers name time)
	       (get-request-by-name request (gethash "name" res))
	     (setf response
		   (if (gethash "body" res)
		       (if inhibit-json-decode
			   (gethash "body" res)
			   (with-xplan-api-json-handlers (json:decode-json-from-string (gethash "body" res))))
		       nil)
		   response-msg (gethash "msg" res)
		   response-code (gethash "code" res)
		   response-headers (gethash "headers" res)
		   time (gethash "time" res)))))
    ;; Now we have populated the Bulk Request object with the results, let's see if any subrequests had an error and throw an exception if they did
    (loop for req across (requests request) do
	 (if (>= (response-code req) 400)
	     (error
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
	    (setf %response (multiple-value-list (%process-request request :do-auth NIL)))
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
	       (setf %response (multiple-value-list (%process-request request :do-auth do-auth)))
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
			  (json:decode-json-from-string (babel:octets-to-string (elt %response 0)))))
		    nil)
       response-headers (elt %response 2)
       response-code (elt %response 1)))
    (setf state :done)))
