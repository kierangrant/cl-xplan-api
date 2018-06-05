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

File: src/testing.lisp
Description: Testing Session for testing and development purposes.
|#

(in-package :cl-xplan-api)

;;; This file is for TESTING AND DEVELOPMENT ONLY
;;; It is not supported for actual usage

(defclass xplan-session-test (xplan-session)
  ((xplanid :initarg :xplanid :initform "")
   (trace :initarg :trace :initform nil)))

(defmethod %process-request ((session xplan-session-test) request &key do-auth)
  (if do-auth (error "Cannot authenticate with an XPLAN-SESSION-TEST session!"))
  (with-slots (drakma-settings) session
    (let ((cookies
	   (make-instance 'drakma:cookie-jar
			  :cookies `(,(make-instance
				       'drakma:cookie
				       :name "XPLANID"
				       :value (slot-value session 'xplanid)
				       :securep nil :http-only-p nil
				       :domain
				       (subseq (elt
						(split-sequence:split-sequence
						 #\: (base-url session))
						1)
					       2)))))
	  (content (get-request-content request))
	  (content-type (get-request-content-type request)))
      (apply
       (lambda (&rest rest)
	 (if (slot-value session 'trace)
	     (progn
	       (format *xplan-api-debug* "--- BEGIN TRACE ---~%Timestamp: ~A~%API Called with:~%~S~%"
		       (rw-ut:write-time-string (get-universal-time) "YYYY-MM-DDThh:mm:ssZ") rest)
	       (force-output *xplan-api-debug*)
	       (let ((response (multiple-value-list (apply *api-call-function* rest))))
		 ;; Let's not output actual content if we can instead decode it!
		 (format *xplan-api-debug* "API Response:~%~S~%" (cdr response))
		 (force-output *xplan-api-debug*)
		 (if (string= (cdr (assoc :content-type (elt response 2))) "application/json")
		     (format *xplan-api-debug* "Response Content:~%~A~%"
			     (babel:octets-to-string
			      (elt response 0)))
		     (format *xplan-api-debug* "Raw Content:~%~A~%"
			     (elt response 0)))
		 (format *xplan-api-debug* "Timestamp: ~A~%--- END TRACE ---~%"
			 (rw-ut:write-time-string (get-universal-time) "YYYY-MM-DDThh:mm:ssZ"))
		 (force-output *xplan-api-debug*)
		 (values-list response)))
	     (apply *api-call-function* rest)))
       (get-request-url request)
       :method (get-request-method request)
       :force-binary T
       :cookie-jar cookies
       :additional-headers `(("Accept" . "application/json, text/html")
			     ("Referer" . ,(base-url session)))
       :user-agent *user-agent*
       (if (and content content-type)
	   (append `(:content ,content :content-type ,content-type) drakma-settings)
	   drakma-settings)))))

(defmacro with-dummy-call (&body body)
  `(let ((*api-call-function*
	  (lambda (&rest rest)
	    (format *xplan-api-debug*
		    "Called with:~%~S~%" rest)
	    (values
	     #.(babel:string-to-octets "[]")
	     200
	     '(("Content-Type" . "application/json"))
	     nil
	     nil
	     nil
	     "OK"))))
     ,@body))

(defmacro with-trace-to-stream ((stream) &body body)
  (let ((api-sym (gensym)) (stream-sym (gensym)))
    `(let* ((,api-sym *api-call-function*) (,stream-sym ,stream)
	    (*api-call-function*
	     (lambda (&rest rest)
	       (format ,stream-sym "--- BEGIN TRACE ---~%Timestamp: ~A~%API Called with:~%~S~%"
		       (rw-ut:write-time-string (get-universal-time) "YYYY-MM-DDThh:mm:ssZ") rest)
	       (force-output ,stream-sym)
	       (let ((response (multiple-value-list (apply ,api-sym rest))))
		 ;; Let's not output actual content if we can instead decode it!
		 (format ,stream-sym "API Response:~%~S~%" (cdr response))
		 (force-output ,stream-sym)
		 (if (string= (cdr (assoc :content-type (elt response 2))) "application/json")
		     (format ,stream-sym "Response Content:~%~A~%"
			     (babel:octets-to-string (elt response 0)))
		     (format ,stream-sym "Raw Content:~%~A~%" (elt response 0)))
		 (format ,stream-sym "Timestamp: ~A~%--- END TRACE ---~%"
			 (rw-ut:write-time-string (get-universal-time) "YYYY-MM-DDThh:mm:ssZ"))
		 (force-output ,stream-sym)
		 (values-list response)))))
       ,@body)))
