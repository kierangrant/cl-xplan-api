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

File: cl-xplan-api.asd
Description: cl-xplan-api ASDF system definition file
|#

(defsystem :cl-xplan-api
    :version (:read-file-form "src/core/VERSION.expr")
    :author "Kieran Grant"
    :license "LLGPL"
    :description "XPlan API Library"
    :components ((:module
		  "src"
		  :components
		  ((:file "package")
		   (:module
		    "core"
		    :depends-on ("package")
		    :components
		    ((:static-file "VERSION.expr")
		     (:file "definitions")
		     (:file "functions" :depends-on ("definitions"))
		     (:file "macros")
		     (:file "classes")
		     (:file "conditions")
		     (:file "methods" :depends-on ("classes" "conditions" "definitions" "macros"))))
		   (:module
		    "api"
		    :components
		    ((:file "access")
		     (:file "asset_class")
		     (:file "assumption_set")
		     (:file "case_manager")
		     (:file "entity")
		     (:file "portfolio")
		     (:file "session")
		     (:file "ufield"))
		    :depends-on ("core" "package")))))
    :depends-on (:drakma :cl-json :babel :decimals :cl-base64 :rw-ut :split-sequence))
