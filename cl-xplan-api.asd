#| -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

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

File: cl-xplan-api.asd
Description: cl-xplan-api ASDF system definition file
|#

(defsystem "cl-xplan-api"
  :version (:read-file-form "src/VERSION.expr")
  :author "Kieran Grant"
  :license "LLGPL"
  :description "XPlan API Library"
  :serial t
  :components ((:file "json-patch")
	       (:module
		"src"
		:components
		((:static-file "VERSION.expr")
		 (:file "package")
		 (:file "definitions")
		 (:file "classes")
		 (:file "functions")
		 (:file "macros")
		 (:file "conditions")
		 (:file "methods"))
		:depends-on ("json-patch")))
  :depends-on ("drakma" "json-to-clos" "babel" "decimals" "cl-base64" "rw-ut" "split-sequence"))
