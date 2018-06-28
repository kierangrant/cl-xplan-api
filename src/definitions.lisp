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

File: src/definitions.lisp
Description: Standard Definitions and overrides for other libraries
|#

(in-package :cl-xplan-api)

;;; INTERNAL API

(defparameter *xplan-api-version*
  #.(with-open-file (str (make-pathname :name "VERSION" :type "expr" :version :newest :defaults (or *compile-file-truename* *load-truename*)) :direction :input)
      (read str))
	      "Version of XPLAN-API Library")
(defparameter *user-agent* (concatenate 'string "CL-XPLAN-API/" *xplan-api-version*) "User-Agent to send to upstream server.")

;;; PUBLIC API

(defparameter *max-rounding* -1024 "Maximum rounding when converting a ratio to a 'BigDeciaml'")
(defvar *xplan-api-debug* *debug-io* "Stream to output debug messages to, if NIL messages are disabled")

(defvar *xplan-session*)
(defvar *xplan-batched-request*)
