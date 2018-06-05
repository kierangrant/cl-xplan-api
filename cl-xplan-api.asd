#| Begining of Extention to ASDF
This is released under the same License as ASDF itself, the MIT-style license included in it.
|#

(defclass recursive-source-module (module)
  ((recursive-source-module-components))) ; We need to have our own hash table to keep track of things

;; We will dynamically calculate this whenever called... this is because simply adding a source file
;; should result in the new file being compiled, without changing anything else
(defmethod component-children ((object recursive-source-module))
  (with-slots (recursive-source-module-components) object
    ;; If cache is not there, create hash-table
    (if (not (slot-boundp object 'recursive-source-module-components))
	(setf recursive-source-module-components (make-hash-table :test 'equal)))
    ;; invalidate all entries
    (maphash (lambda (k v) (declare (ignore k)) (setf (getf v :valid) nil)) recursive-source-module-components)
    ;; go through directory and add missing files, validate still existing files
    (loop
       for file in (directory
		    (merge-pathnames
		     (make-pathname :directory '(:relative :wild-inferiors)
				    :name :wild :type "lisp")
		     (component-pathname object)))
       do
	 (let ((name (enough-namestring (make-pathname :type nil :defaults file) (component-pathname object))))
	   (if (not (elt (multiple-value-list (gethash name recursive-source-module-components)) 1))
	       (setf (gethash name recursive-source-module-components)
		     (list :component
			   (make-instance
			    'cl-source-file
			    :parent object
			    :name name
			    :pathname file))))
	   (setf (getf (gethash name recursive-source-module-components) :valid) t)))
    ;; remove invalid entries
    (maphash (lambda (k v) (if (not (getf v :valid)) (remhash k recursive-source-module-components)))
	     recursive-source-module-components)
    ;; Now return a list
    (loop for val being the hash-value in recursive-source-module-components collecting
	 (getf val :component))))

;; we actually don't support set'ing the children for this type, it is a pseudo-module
(defmethod module-components ((object recursive-source-module)) (component-children object))

;; We re-create this hash table every time, the actual components shouldn't be recreated unnecssarily
(defmethod component-children-by-name ((parent recursive-source-module))
  (let ((hash (make-hash-table :test 'equal)))
    ;; This call to component-children will update cache if needed
    (loop :for c :in (component-children parent)
       :for name = (component-name c)
       :for previous = (gethash name hash)
       :do (when previous (error 'duplicate-names :name name))
       (setf (gethash name hash) c))
    hash))

(defmethod asdf/component::module-components-by-name ((object recursive-source-module))
  (component-children-by-name object))

#| End of ASDF Extention |#

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

File: cl-xplan-api.asd
Description: cl-xplan-api ASDF system definition file
|#

(defsystem "cl-xplan-api"
  :version (:read-file-form "src/core/VERSION.expr")
  :author "Kieran Grant"
  :license "LLGPL"
  :description "XPlan API Library"
  :components ((:file "json-patch")
	       (:module
		"src"
		:components
		((:file "package")
		 (:module
		  "core"
		  :depends-on ("package")
		  :components
		  ((:static-file "VERSION.expr")
		   (:file "definitions")
		   (:file "classes")
		   (:file "functions" :depends-on ("definitions" "classes"))
		   (:file "macros")
		   (:file "conditions")
		   (:file "methods" :depends-on ("classes" "conditions" "definitions" "macros"))))
		 (:recursive-source-module "api" :depends-on ("core" "package")))
		:depends-on ("json-patch")))
  :depends-on ("drakma" "json-t-clos" "babel" "decimals" "cl-base64" "rw-ut" "split-sequence" "fstring-reader"))
