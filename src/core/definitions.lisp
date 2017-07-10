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

File: src/definitions.lisp
Description: Standard Definitions and overrides for other libraries
|#

(in-package :cl-xplan-api/core)

;;; INTERNAL API

(defparameter *xplan-api-version*
  #.(with-open-file (str (make-pathname :name "VERSION" :type "expr" :version :newest :defaults *compile-file-truename*) :direction :input)
      (read str))
	      "Version of XPLAN-API Library")
(defparameter *user-agent* (concatenate 'string "CL-XPLAN-API/" *xplan-api-version*) "User-Agent to send to upstream server.")
(defparameter *api-call-function* #'drakma:http-request "Low-Level HTTP Request function to make API request.")

;;; PUBLIC API

(defparameter *max-rounding* -1024 "Maximum rounding when converting a ratio to a 'BigDeciaml'")
(defvar *xplan-api-debug* *debug-io* "Stream to output debug messages to, if NIL messages are disabled")
