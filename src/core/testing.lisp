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

File: src/core/testing.lisp
Description: Testing Session for testing and development purposes.
|#

(in-package :cl-xplan-api/core)

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
      (if (slot-value session 'trace) (format *debug-io* "~S~%" (drakma:cookie-jar-cookies cookies)))
    (apply
     (lambda (&rest rest)
       (if (slot-value session 'trace)
	   (progn
	     (format *debug-io* "API Called with:~%~S~%" rest)
	     (let ((response (multiple-value-list (apply *api-call-function* rest))))
	       (format *debug-io* "API Response:~%~S~%" response)
	       (if (string= (cdr (assoc :content-type (elt response 2))) "application/json")
		   (format *debug-io* "Decoded Content:~%~S~%"
			   (babel:octets-to-string
			    (elt response 0))))
	       (values-list response)))
	   (apply *api-call-function* rest)))
     (get-request-url request)
     :method (get-request-method request)
     :force-binary T
     :cookie-jar cookies
     :additional-headers `(("Accept" . "application/json") ("Referer" . ,(base-url session)))
     :user-agent *user-agent*
     (if (and content content-type)
	 (append `(:content ,content :content-type ,content-type) drakma-settings)
	 drakma-settings)))))
