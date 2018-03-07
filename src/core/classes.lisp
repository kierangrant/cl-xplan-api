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

File: src/core/classes.lisp
Description: XPLAN API Classes
|#

(in-package :cl-xplan-api/core)

(defclass xplan-session ()
  ((drakma-settings :accessor drakma-settings :initarg :drakma-settings :initform '(:verify :required) :documentation "Settings to control drakma connection")
   (api-key :accessor api-key :initarg :api-key :documentation "API Key used to authenticate with XPlan")
   (username :accessor username :initarg :username :documentation "Username to use when authenticating")
   (password :accessor password :initarg :password :documentation "Password to use when authenticating")
   (session-state :accessor session-state :initarg :session-state :initform (make-instance 'drakma:cookie-jar) :documentation "Session state used by backend to keep track of session")
   (base-url :accessor base-url :initarg :base-url :documentation "Base site url used when making requests.")
   (transport-version :accessor transport-version :initarg :transport-version :initform 1 :documentation "XPlan API Transport Version")
   (auto-reauth :accessor xplan-session-auto-reauth :initarg :auto-reauth :initform T :documentation "Automatically reauthenticated on expiry or invalidation?")
   (force-init-auth :accessor force-init-auth :initarg :force-init-auth :initform NIL :documentation "Force Authentication on first request")))

(defclass xplan-request ()
  ((response :accessor response :initform NIL :documentation "Response of request, NIL if no response.")
   (response-code :accessor response-code :initform 0 :documentation "HTTP Response code, check this for sucesses of failure.")
   (response-headers :accessor response-headers :initform NIL :documentation "HTTP Response Headers (if any)")
   (resource :accessor resource :initarg :resource :documentation "Resource of request, ommit the \"/resourceful\" part.")
   (method :accessor request-method :initarg :method :initform :get)
   (parameters :accessor parameters :initarg :parameters :initform NIL
	       :documentation "Must be an a-list, if value if a sequence, it is broken down to URL array format: EG: ((\"fields\" . #(\"foo\" \"bar\"))) -> \"?fields.0=foo&fields.1=bar\"")
   (state :initform :prepare)
   (session :accessor session :initarg :session :documentation "xplan-session object this request uses")
   (content :accessor content :initarg :content :initform NIL :documentation "Body of request (POST only)")
   (content-type :accessor content-type :initarg :content-type :initform NIL :documentation "Content-type of Body content (POST only)")
   (request-uri)))

(defclass xplan-request-bulk-requests ()
  ((response :accessor response :initform NIL)
   (response-msg  :accessor response-msg :initform NIL)
   (response-code :accessor response-code :initform 0)
   (response-headers :accessor response-headers :initform NIL)
   (resource :accessor resource :initarg :resource :documentation "Resource of request, ommit the \"/resourceful\" part.")
   (method :accessor request-method :initarg :method :initform :get)
   (parameters :accessor parameters :initarg :parameters :initform NIL)
   (name :accessor name :initarg :name :documentation "Name for this request")
   (omit-results-on-success :accessor omit-results-on-success :initform T :initarg :omit-results-on-success :documentation "For a stand-alone request, the response will be omitted only when this value is explicitly specified to True. For a dependent request, the response will be omitted unless this value is explicitly specified to false.")
   (time :accessor response-time :initform NIL :documentation "Time spent on this request, if reported")
   (inhibit-json-decode-default :initarg :inhibit-json-decode-default :initform NIL :documentation "Default value to use in process-request for inhibit-json-decode if user doesn't supply a value.")))

;; requests is array of actual requests, the requests themselves store the response
;; state is one of :prepare :processing :done
(defclass xplan-request-bulk ()
  ((response-code :initform 0)
   (requests :accessor requests :initform (make-array 0 :element-type 'xplan-request-bulk-requests :adjustable T :fill-pointer 0) :initarg :requests)
   (state :initform :prepare)
   (session :accessor session :initarg :session :documentation "xplan-session object that this request uses")
   (include-subtimings :accessor include-subtimings :initarg :include-subtimings :initform NIL :documentation "Include Subtimings of processing in responses")))
