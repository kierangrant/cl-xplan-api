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

File: src/macros.lisp
Description: Client and API Macros
|#

(in-package :cl-xplan-api/core)

;;; INTERNAL API

;; cond-hash - Conditionally build-up hash-table. Body is of forms (cond subitem-str &optional (value cond))
(defmacro cond-hash (&body items)
  (let ((struct (gensym)))
    `(let ((,struct (make-hash-table :test #'equal)))
       ,@(loop for item in items collecting
	      (destructuring-bind (cond subitem-str &optional (value cond)) item
		`(if ,cond (setf (gethash ,subitem-str ,struct) ,value))))
       ,struct)))

;; Define an API entrypoint.

#|
Defines an API entry point.

extra-parms are extra keyword parameters for use in expressions for different parameters, used for parameters that MUST NOT come out in API parameters call.

hidden-single-parameters are parameters that are added to Single API call method, for such things like needing '?_method=...' in a request that has parameters. Is an alist of name, value pairs, that are inserted in a form that allows name and value to be run-time expressions
hidden-bulk-parameters is like hidden-single-parameters, but it's parameters are added like arglist, as nothing is every added to URI in a bulk request, you can customize that using bulk-resource

single-parms-as-body forces paramters to instead by inserted as content.
Content-Type will be 'application/json'. The content of parameters is convert to JSON using
json:encode-json-to-string. You must convert to XPLAN Types yourself where needed.

{single-,bulk-}content and {single-,bulk-}content-type are overrides, they override all normal processing of arglist. If supplied, the following parameters to define-entrypoint are ignored:
hidden-single-parameters, hidden-bulk-parameters, single-parms-as-body and inhibit-transaction.
You must also specify content-type if you use these fields.
If you specify content and content-type, it is default for single/bulk content and content-type.
If you do not specify content and content-type but specify one of the single-/bulk- variety, the one
you did not specify (as long as not inhibited by inhibit-single or inhibit-bulk) will process arguments
like normal.

{,single-,bulk-}inhibit-json-decode sets default value for inhibit-json-decode.

arglist is of form ({field|(field &key string cond value documentation)}*):

field is as per lambda list as a keyword argument. Thus instead of a symbol it could be a list, in which case (car field) will be taken as field for the defaults for following.
string, if given will provide the string to use in the field, defaults to (STRING-DOWNCASE (SYMBOL-NAME field))
cond is expression used to determine if field will be included, defaults to field
value is expression to use for value of field, defaults to field

documentation in arglist is documentation for that particular parameter. Currently this does nothing, but in the future it may be possible to look this information up.

Examples of an arglist entry:
field -> (if field `(("field" . ,field)))
(field) -> (if field `(("field" . ,field)))
((field nil field-p)) -> (if field `(("field" . ,field))) ; will not automatically use field-p
(field :string "Field" :value (symbol-name field)) -> (if field `(("Field" . ,(symbol-name field))))
((field "Something" field-p) :string "Field_Thing" :cond (and field-p (not (string= field "Blarg"))) :value (somefunc field)) -> (if (and field-p (not (string= field "Blarg"))) `(("Field_Thing" . ,(somefunc field))))
|#
(defmacro define-entrypoint (name method (&rest extra-parms) (&rest arglist)
			     &key documentation resource inhibit-bulk inhibit-single
			       inhibit-transaction single-parms-as-body (single-method method)
			       (single-resource resource) (bulk-method method) (bulk-resource resource)
			       hidden-single-parameters hidden-bulk-parameters
			       (content nil content-p) content-type inhibit-json-decode
			       (single-content content single-content-p)
			       (single-content-type content-type)
			       (single-inhibit-json-decode inhibit-json-decode)
			       (bulk-content content bulk-content-p)
			       (bulk-content-type content-type)
			       (bulk-inhibit-json-decode inhibit-json-decode))
  (if (not (eq :external (elt (multiple-value-list (find-symbol (symbol-name name))) 1)))
      (warn "Symbol ~S is not external in package ~A" name (package-name *package*)))
  ;; If content is provided, set {single/bulk}-content-p as if manually provided
  ;; *-content-type is required if *-content is provided !!
  (let* ((field-entries (loop for item in arglist collecting (if (typep item 'symbol) item (car item))))
	 sparms bparms)
    (if content-p
	(setf single-content-p t
	      bulk-content-p t))
    (if (and bulk-content-p (not (string= bulk-content-type "application/json")))
	(error "XPLAN API Batch request currently only support JSON body"))
    (macrolet
	((parm-processor (inhibit hidden)
	   `(if (not ,inhibit)
		`(let ((h (make-hash-table :test #'equal)))
		   ,@(if (not inhibit-transaction)
			 `((if request-transaction
			       (setf (gethash "_transaction" h) request-transaction))))
		   ,@(loop for item in ,hidden collecting
			  `(setf (gethash ,(car item) h) ,(cdr item)))
		   ,@(loop for item in arglist collecting
			  (let (s c v)
			    (if (typep item 'symbol)
				(setf s (string-downcase (symbol-name item))
				      c item
				      v item)
				(destructuring-bind
				      (f &key (string nil s-p) (cond f c-p) (value f v-p) documentation) item
				  (declare (ignore documentation))
				  (setf
				   s
				   (if s-p string
				       (string-downcase
					(symbol-name
					 (if (typep f 'symbol) f (car f)))))
				   c
				   (if c-p cond (if (typep f 'symbol) f (car f)))
				   v
				   (if v-p value (if (typep f 'symbol) f (car f))))))
			    `(if ,c (setf (gethash ,s h) ,v))))
		   h))))
      (setf sparms (parm-processor inhibit-single hidden-single-parameters)
	    bparms (parm-processor inhibit-bulk hidden-bulk-parameters)))
    `(progn
       ,@(if (not (boundp name))
	     `((defgeneric ,name (session method &key &allow-other-keys))))
       ,@(if (not inhibit-single)
	     `((defmethod ,name ((session xplan-session) (method (eql ,method))
				 &key ,@(if (not (or inhibit-transaction single-content-p))
					    '(request-transaction))
				   inhibit-auth
				   (inhibit-json-decode ,single-inhibit-json-decode)
				   return-request ,@extra-parms ,@field-entries)
		 ,@(if documentation `(,documentation))
		 (let ((res
			,(if single-content-p
			     `(xplan-api-call
			       session
			       :inhibit-auth inhibit-auth
			       :inhibit-json-decode inhibit-json-decode
			       :method ,single-method
			       :resource ,single-resource
			       :content ,single-content
			       :content-type ,single-content-type)
			     `(xplan-api-call
			       session
			       :inhibit-auth inhibit-auth
			       :inhibit-json-decode inhibit-json-decode
			       :method ,single-method
			       :resource ,single-resource
			       ,@(if (not single-parms-as-body) `(:parameters ,sparms))
			       ,@(if single-parms-as-body
				     `(:content-type "application/json"
						     :content (json:encode-json-to-string ,sparms)))))))
		   (if return-request
		       res
		       (response res))))))
       ,@(if (not inhibit-bulk)
	     `((defmethod ,name ((session xplan-request-bulk) (method (eql ,method))
				 &key request-name
				   ,@(if (not (or inhibit-transaction bulk-content-p))
					 '(request-transaction))
				   (inhibit-json-decode ,bulk-inhibit-json-decode)
				   ,@extra-parms ,@field-entries)
		 ,@(if documentation `(,documentation))
		 (prepare-request
		  session
		  :name request-name
		  :method ,bulk-method
		  :resource ,bulk-resource
		  :inhibit-json-decode inhibit-json-decode
		  :parameters ,(if bulk-content-p bulk-content bparms))))))))

#| define-dynamiclike-entrypoints
Creates entrypoints of the same style of <dynamic> but for named handlers
Used to create of form item-prefix/:enttiy_id/item-name/...{/item-postfix} for specified requests
Parameters:
name              - name of entrypoint to create
item-name         - name of specific item being created
item-prefix       - prefix of entrypoint url (eg: entity/client or entity/user-v2)
subitem-name      - Subitem Name. If non-NILL, changes output to be for a subitem of item-name.
item-postfix      - postfix of entrypoint url (eg entity/client/somethnig/'attachment')

subitem-name is useful for "/entity/client/:entity_id/item-name/:list_obj_index/subitem-name/...

Extra Options
extra-args        - Extra arguments for requests
list-obj-field    - Field for list obj. defaults to 'list_obj_index
sublist-obj-field - Field for sublist obj. defaults to 'sublist_obj_index
default-args      - Default for arguments for request*

Extra options are used in request-defaults, get-defaults, post-defaults, put-defaults, patch-defaults and delete-defeaults
with request-defaults setting values for all if thier value is omitted.

*default-args is only available on get-defaults, post-defaults, patch-defaults, patch-defaults and delete-defaults
|#
(defmacro define-dynamiclike-entrypoints
    ((name item-name item-prefix &optional subitem-name item-postfix)
     &key request-defaults get-defaults post-defaults put-defaults patch-defaults delete-defaults)
  (destructuring-bind  (&key ((:extra-args extra-args-default))
			     ((:list-obj-field list-obj-field-default) 'list_obj_index)
			     ((:sublist-obj-field sublist-obj-field-default) 'sublist_obj_index)
			     ((:inhibit inhibit-default)))
      request-defaults
  (destructuring-bind  (&key ((:extra-args get-extra-args) extra-args-default)
			     ((:list-obj-field get-list-obj-field) list-obj-field-default)
			     ((:sublist-obj-field get-sublist-obj-field) sublist-obj-field-default)
			     ((:default-args get-default-args)
			      (if subitem-name
				  `((indexes :cond (and (not ,get-sublist-obj-field) indexes))
				    fields
				    (page :cond (and (not ,get-sublist-obj-field) page)))
				  `((indexes :cond (and (not ,get-list-obj-field) indexes))
				    fields
				    (page :cond (and (not ,get-list-obj-field) page)))))
			     ((:inhibit get-inhibit) inhibit-default))
      get-defaults
  (destructuring-bind  (&key ((:extra-args post-extra-args) extra-args-default)
			     ((:list-obj-field post-list-obj-field) list-obj-field-default)
			     ((:default-args post-default-args) '(fields extra_return_fields))
			     ((:inhibit post-inhibit) inhibit-default))
      post-defaults
  (destructuring-bind (&key ((:extra-args put-extra-args) extra-args-default)
			    ((:list-obj-field put-list-obj-field) list-obj-field-default)
			    ((:default-args put-default-args) '(fields extra_return-fields))
			    ((:inhibit put-inhibit) inhibit-default))
      put-defaults
  (destructuring-bind  (&key ((:extra-args patch-extra-args) extra-args-default)
			     ((:list-obj-field patch-list-obj-field) list-obj-field-default)
			     ((:sublist-obj-field patch-sublist-obj-field) sublist-obj-field-default)
			     ((:default-args patch-default-args) '(fields extra_return_fields))
			     ((:inhibit patch-inhibit) inhibit-default))
      patch-defaults
  (destructuring-bind (&key ((:extra-args delete-extra-args) extra-args-default)
			    ((:list-obj-field delete-list-obj-field) list-obj-field-default)
			    ((:sublist-obj-field delete-sublist-obj-field) sublist-obj-field-default)
			    ((:default-args delete-default-args))
			    ((:inhibit delete-inhibit) inhibit-default))
      delete-defaults
    `(progn
   ,@(if (not get-inhibit)
     `((define-entrypoint ,name :get
	 ,(if subitem-name
	      `(entity_id ,get-list-obj-field ,get-sublist-obj-field)
	      `(entity_id ,get-list-obj-field))
	 ,(append get-default-args get-extra-args)
	 :resource
	 ,(if subitem-name
	      `(format nil
		       ,(format nil "~A/~~A/~A/~~A/~A~~@[/~~A~~]~@[/~A~]" item-prefix item-name subitem-name item-postfix)
		       entity_id ,get-list-obj-field ,get-sublist-obj-field)
	      `(format nil
		       ,(format nil "~A/~~A/~A/~~@[/~~A~~]~@[/~A~]" item-prefix item-name item-postfix)
		       entity_id ,get-list-obj-field)))))
   ,@(if (not post-inhibit)
     `((define-entrypoint ,name :post
	 ,(if subitem-name `(entity_id ,post-list-obj-field) '(entity_id))
	 ,(append post-default-args post-extra-args)
	 :resource
	 ,(if subitem-name
	      `(format nil ,(format nil "~A/~~A/~A/~~A/~A~@[/~A~]" item-prefix item-name subitem-name item-postfix)
		       entity_id ,post-list-obj-field)
	      `(format nil ,(format nil "~A/~~A/~A~@[/~A~]" item-prefix item-name item-postfix) entity_id)))))
   ,@(if (not put-inhibit)
     `((define-entrypoint ,name :put
	 ,(if subitem-name `(entity_id ,put-list-obj-field) '(entity_id))
	 ,(append put-default-args put-extra-args)
	 :resource
	 ,(if subitem-name
	      `(format nil ,(format nil "~A/~~A/~A/~~A/~A~@[/~A~]" item-prefix item-name subitem-name item-postfix) entity_id ,put-list-obj-field)
	      `(format nil ,(format nil "~A/~~A/~A~@[/~A~]" item-prefix item-name item-postfix) entity_id)))))
   ,@(if (not patch-inhibit)
     `((define-entrypoint ,name :patch
	 ,(if subitem-name `(entity_id ,patch-list-obj-field ,patch-sublist-obj-field) `(entity_id ,patch-list-obj-field))
	 ,(append patch-default-args patch-extra-args)
	 :resource
	 ,(if subitem-name
	      `(format nil ,(format nil "~A/~~A/~A/~~A/~A/~~A~@[/~A~]" item-prefix item-name subitem-name item-postfix) entity_id ,patch-list-obj-field ,patch-sublist-obj-field)
	      `(format nil ,(format nil "~A/~~A/~A/~~A~@[/~A~]" item-prefix item-name item-postfix) entity_id ,patch-list-obj-field)))))
   ,@(if (not delete-inhibit)
     `((define-entrypoint ,name :delete
	 ,(if subitem-name `(entity_id ,delete-list-obj-field ,delete-sublist-obj-field) `(entity_id ,delete-list-obj-field))
	 ,(append delete-default-args delete-extra-args)
	 :resource
	 ,(if subitem-name
	      `(format nil ,(format nil "~A/~~A/~A/~~A/~A/~~A~@[/~A~]" item-prefix item-name subitem-name item-postfix) entity_id ,delete-list-obj-field ,delete-sublist-obj-field)
	      `(format nil ,(format nil "~A/~~A/~A/~~A~@[/~A~]" item-prefix item-name item-postfix) entity_id ,delete-list-obj-field)))))))))))))

(defmacro with-xplan-api-json-handlers (&body body)
  `(let ((json:*beginning-of-object-handler* #'object-begin)
	 (json:*end-of-object-handler* #'object-end)
	 (json:*object-key-handler* #'object-key)
	 (json:*object-value-handler* #'object-value)
	 (json:*json-array-type* 'vector))
     ,@body))

;;; PUBLIC

(defmacro with-xplan-session
    ((session-name &optional (session-spec NIL) (keep-open NIL) &rest session-opt) &body body)
  "Executes body inside a unwind-protect to optionally close-session if keep-open in NIL, creating session if one isn't provided by session-spec using session-opt as options for session. Errors are ignored when automatically deleting session."
  (let ((ko (gensym)))
    `(let ((,ko ,keep-open)
	   (,session-name ,(if session-spec session-spec `(make-instance 'xplan-session ,@session-opt))))
       (unwind-protect
	    (progn ,@body)
	 (when (not ,ko) (ignore-errors (delete-session ,session-name)))))))

(defmacro with-bulk-request ((request-name session &optional (request-spec NIL) &rest request-opt) &body body)
  "Executes body in a let environment where a BULK request is created to request-name if not specified by request-spec using request-opt as options."
  `(let ((,request-name
	  ,(if request-spec
	       request-spec
	       `(make-instance
		 'xplan-request-bulk
		 :session ,session
		 ,@request-opt))))
     ,@body))
