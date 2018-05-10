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

File: src/core/macros.lisp
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
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (export ',name (find-package :cl-xplan-api/api))
	 (import ',name (find-package :cl-xplan-api))
	 (export ',name (find-package :cl-xplan-api)))
       ,@(if (not (fboundp name))
	     `((defgeneric ,name (session method &key))))
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
extra-args           - Extra arguments for requests
list-obj-field       - Field for list obj. defaults to 'list_obj_index
sublist-obj-field    - Field for sublist obj. defaults to 'sublist_obj_index
inhibit-subitem-name - Inhibits the output of "/subitem-name" in URL's. But still allow sublist items.
                       Eg: /entity/client-v3/:entity_id/attachment/:field/:attach_id**
default-args         - Default for arguments for request*
inhibit-itemid       - Inhibits the output of the item ID in the url.
                       EG: just GET /entity/client-v3/:entity_id/thing with "/:thing_id" ommited ***
inhibit-subitemid    - Inhibits the output of the subitem ID in the url.
                       EG: just GET /entity/client-v3/:entity_id/thing/:thing_id/subthing with
                       "/:subthing_id" omiited. If you want to remove "/:thing_id" as well use
                       inhibit-itemid ***

Extra options are used in request-defaults, get-defaults, post-defaults, put-defaults, patch-defaults and delete-defeaults
with request-defaults setting values for all if thier value is omitted.

*default-args is only available on get-defaults, post-defaults, put-defaults, patch-defaults and delete-defaults
*** Used for things that there is maybe only 1 of, or if it can only by requested by a passed paramter such as thing_ids, and not individually by /:thing_id
|#
(defmacro define-dynamiclike-entrypoints
    ((name item-name item-prefix &optional subitem-name item-postfix)
     &key request-defaults get-defaults post-defaults put-defaults patch-defaults delete-defaults)
  (destructuring-bind  (&key ((:extra-args extra-args-default))
			     ((:list-obj-field list-obj-field-default) 'cl-xplan-api/api::list_obj_index)
			     ((:sublist-obj-field sublist-obj-field-default) 'cl-xplan-api/api::sublist_obj_index)
			     ((:inhibit inhibit-default))
			     ((:inhibit-subitem-name inhibit-subitem-name-default))
			     ((:inhibit-itemid inhibit-itemid-default))
			     ((:inhibit-subitemid inhibit-subitemid-default))
			     ((:single-parms-as-body single-parms-as-body-default)))
      request-defaults
  (destructuring-bind  (&key ((:extra-args get-extra-args) extra-args-default)
			     ((:list-obj-field get-list-obj-field) list-obj-field-default)
			     ((:sublist-obj-field get-sublist-obj-field) sublist-obj-field-default)
			     ((:default-args get-default-args)
			      (if subitem-name
				  `((cl-xplan-api/api::indexes :cond (and (not ,get-sublist-obj-field) cl-xplan-api/api::indexes))
				    cl-xplan-api/api::fields
				    (cl-xplan-api/api::page :cond (and (not ,get-sublist-obj-field) cl-xplan-api/api::page)))
				  `((cl-xplan-api/api::indexes :cond (and (not ,get-list-obj-field) cl-xplan-api/api::indexes))
				    cl-xplan-api/api::fields
				    (cl-xplan-api/api::page :cond (and (not ,get-list-obj-field) cl-xplan-api/api::page)))))
			     ((:inhibit get-inhibit) inhibit-default)
			     ((:inhibit-subitem-name get-inhibit-subitem-name) inhibit-subitem-name-default)
			     ((:inhibit-itemid get-inhibit-itemid) inhibit-itemid-default)
			     ((:inhibit-subitemid get-inhibit-subitemid) inhibit-subitemid-default))
      get-defaults
  (destructuring-bind  (&key ((:extra-args post-extra-args) extra-args-default)
			     ((:list-obj-field post-list-obj-field) list-obj-field-default)
			     ((:default-args post-default-args) '(fields extra_return_fields))
			     ((:inhibit post-inhibit) inhibit-default)
			     ((:inhibit-subitem-name post-inhibit-subitem-name) inhibit-subitem-name-default)
			     ((:inhibit-itemid post-inhibit-itemid) inhibit-itemid-default)
			     ((:single-parms-as-body post-single-parms-as-body) single-parms-as-body-default))
      post-defaults
  (destructuring-bind (&key ((:extra-args put-extra-args) extra-args-default)
			    ((:list-obj-field put-list-obj-field) list-obj-field-default)
			    ((:default-args put-default-args) '(fields extra_return-fields))
			    ((:inhibit put-inhibit) inhibit-default)
			    ((:inhibit-subitem-name put-inhibit-subitem-name) inhibit-subitem-name-default)
			    ((:inhibit-itemid put-inhibit-itemid) inhibit-itemid-default)
			    ((:single-parms-as-body put-single-parms-as-body) single-parms-as-body-default))
      put-defaults
  (destructuring-bind (&key ((:extra-args patch-extra-args) extra-args-default)
			    ((:list-obj-field patch-list-obj-field) list-obj-field-default)
			    ((:sublist-obj-field patch-sublist-obj-field) sublist-obj-field-default)
			    ((:default-args patch-default-args) '(fields extra_return_fields))
			    ((:inhibit patch-inhibit) inhibit-default)
			    ((:inhibit-subitem-name patch-inhibit-subitem-name) inhibit-subitem-name-default)
			    ((:inhibit-itemid patch-inhibit-itemid) inhibit-itemid-default)
			    ((:inhibit-subitemid patch-inhibit-subitemid) inhibit-subitemid-default)
			    ((:single-parms-as-body patch-single-parms-as-body) single-parms-as-body-default))
      patch-defaults
  (destructuring-bind (&key ((:extra-args delete-extra-args) extra-args-default)
			    ((:list-obj-field delete-list-obj-field) list-obj-field-default)
			    ((:sublist-obj-field delete-sublist-obj-field) sublist-obj-field-default)
			    ((:default-args delete-default-args))
			    ((:inhibit delete-inhibit) inhibit-default)
			    ((:inhibit-subitem-name delete-inhibit-subitem-name) inhibit-subitem-name-default)
			    ((:inhibit-itemid delete-inhibit-itemid) inhibit-itemid-default)
			    ((:inhibit-subitemid delete-inhibit-subitemid) inhibit-subitemid-default))
      delete-defaults
    `(progn
   ,@(if (not get-inhibit)
     `((define-entrypoint ,name :get
	 (cl-xplan-api/api::entity_id
	  ,@(if (not get-inhibit-itemid) `(,get-list-obj-field))
	  ,@(if (and subitem-name (not get-inhibit-subitemid)) `(,get-sublist-obj-field)))
	 ,(append get-default-args get-extra-args)
	 :resource
	 ;; item-prefix/:entity_id/item-name/:list-obj-field/subitem-name/:sublist-obj-field/item-postfix
	 ;; list-obj-field is ommited if get-inhibit-itemid is T
	 ;; subitem-name and sublist-obj-field is only there if subitem-name has a value
	 ;; subitem-name is ommited if get-inhibit-subitem-name is T
	 ;; sublist-obj-field is ommited if get-inhibit-subitemid is T
	 (format nil
		 ,(format nil "~{~A~}"
			  `(,item-prefix "/"
			    "~A/" ; entity_id
			    ,item-name
			    ,@(if (and (not subitem-name) (not get-inhibit-itemid))
				  '("~@[/~A~]")) ; get-list-obj-field
			    ,@(if (and subitem-name (not get-inhibit-itemid))
				  '("/~A")) ; get-list-obj-field
			    ,@(if (and subitem-name (not get-inhibit-subitem-name))
				  `("/" ,subitem-name)) ; subitem-name
			    ,@(if (and subitem-name (not get-inhibit-subitemid))
				  '("~@[/~A~]")) ; get-sublist-obj-field
			    ,@(if item-postfix `("/" ,item-postfix))))
		 cl-xplan-api/api::entity_id
		 ,@(if (not get-inhibit-itemid) `(,get-list-obj-field))
		 ,@(if (and subitem-name (not get-inhibit-subitemid)) `(,get-sublist-obj-field))))))
   ,@(if (not post-inhibit)
     `((define-entrypoint ,name :post
	 (cl-xplan-api/api::entity_id
	  ,@(if (and subitem-name (not post-inhibit-itemid)) `(,post-list-obj-field)))
	 ,(append post-default-args post-extra-args)
	 :single-parms-as-body ,post-single-parms-as-body
	 :resource
	 ;; item-prefix/:entity_id/item-name/:list-obj-field/subitem-name/item-postfix
	 ;; list-obj-field is ommited if post-inhibit-itemid is T. It will only appear if subitem-name has a value
	 ;; subitem-name is ommited if post-inhibit-subitem-name is T
	 (format nil
		 ,(format nil "~{~A~}"
			  `(,item-prefix "/"
			    "~A/" ; entity_id
			    ,item-name
			    ,@(if (and subitem-name (not post-inhibit-itemid))
				  '("/~A")) ; post-list-obj-field
			    ,@(if (and subitem-name (not post-inhibit-subitem-name))
				  `("/" ,subitem-name))
			    ,@(if item-postfix `("/" ,item-postfix))))
		 cl-xplan-api/api::entity_id
		 ,@(if (and subitem-name (not post-inhibit-itemid))
		       `(,post-list-obj-field))))))
   ,@(if (not put-inhibit)
     `((define-entrypoint ,name :put
	 (cl-xplan-api/api::entity_id
	  ,@(if (and subitem-name (not put-inhibit-itemid)) `(,put-list-obj-field)))
	 ,(append put-default-args put-extra-args)
	 :single-parms-as-body ,put-single-parms-as-body
	 :resource
	 ;; item-prefix/:entity_id/item-name/:list-obj-field/subitem-name/item-postfix
	 ;; list-obj-field is ommited if put-inhibit-itemid is T. It will only appear if subitem-name has a value
	 ;; subitem-name is ommited if put-inhibit-subitem-name is T
	 (format nil
		 ,(format nil "~{~A~}"
			  `(,item-prefix "/"
			    "~A/" ; entity_id
			    ,item-name
			    ,@(if (and subitem-name (not put-inhibit-itemid))
				  '("/~A")) ; put-list-obj-field
			    ,@(if (and subitem-name (not put-inhibit-subitem-name))
				  `("/" ,subitem-name))
			    ,@(if item-postfix `("/" ,item-postfix))))
		 cl-xplan-api/api::entity_id
		 ,@(if (and subitem-name (not put-inhibit-itemid))
		       `(,put-list-obj-field))))))
   ,@(if (not patch-inhibit)
     `((define-entrypoint ,name :patch
	 (cl-xplan-api/api::entity_id
	  ,@(if (not patch-inhibit-itemid) `(,patch-list-obj-field))
	  ,@(if (and subitem-name (not patch-inhibit-subitemid)) `(,patch-sublist-obj-field)))
	 ,(append patch-default-args patch-extra-args)
	 :single-parms-as-body ,patch-single-parms-as-body
	 :resource
	 ;; item-prefix/:entity_id/item-name/:list-obj-field/subitem-name/:subitem-obj-field/postfix
	 ;; list-obj-field is ommited if patch-inhibit-itemid is T
	 ;; subitem-name appears if subitem-name has value and patch-inhibit-subitem-name is NIL
	 ;; subitem-obj-field is ommited if patch-inhibit-subitemid is T
	 (format nil
		 ,(format nil "~{~A~}"
			  `(,item-prefix "/"
			    "~A/" ; entity_id
			    ,item-name
			    ,@(if (not patch-inhibit-itemid) '("/~A")) ; patch-list-obj-field
			    ,@(if (and subitem-name (not patch-inhibit-subitem-name))
				  `("/" ,subitem-name))
			    ,@(if (and subitem-name (not patch-inhibit-subitemid))
				  '("/~A")) ; patch-sublist-obj-field
			    ,@(if item-postfix `("/" ,item-postfix))))
		 cl-xplan-api/api::entity_id
		 ,@(if (not patch-inhibit-itemid) `(,patch-list-obj-field))
		 ,@(if (and subitem-name (not patch-inhibit-subitemid)) `(,patch-sublist-obj-field))))))
   ,@(if (not delete-inhibit)
     `((define-entrypoint ,name :delete
	 (cl-xplan-api/api::entity_id
	  ,@(if (not delete-inhibit-itemid) `(,delete-list-obj-field))
	  ,@(if (and subitem-name (not delete-inhibit-subitemid)) `(,delete-sublist-obj-field)))
	 ,(append delete-default-args delete-extra-args)
	 :resource
	 ;; item-prefix/:entity_id/item-name/:list-obj-field/subitem-name/:sublist-obj-field/item-postfix
	 ;; list-obj-field is ommited if delete-inhibit-itemid is T
	 ;; subitem-name is only there is subitem-name has a value and delete-inhibit-subitem-name is NIL
	 ;; sublist-obj-field is ommited if delete-inhibit-subitemid is T
	 (format nil
		 ,(format nil "~{~A~}"
			  `(,item-prefix "/"
			    "~A/" ; entity_id
			    ,item-name
			    ,@(if (not delete-inhibit-itemid) '("/~A")) ; delete-list-obj-field
			    ,@(if (and subitem-name (not delete-inhibit-subitem-name))
				  `("/" ,subitem-name))
			    ,@(if (and subitem-name (not delete-inhibit-subitemid))
				  `("/~A")) ; delete-sublist-obj-field
			    ,@(if item-postfix `("/" ,item-postfix))))
		 cl-xplan-api/api::entity_id
		 ,@(if (not delete-inhibit-itemid) `(,delete-list-obj-field))
		 ,@(if (and subitem-name (not delete-inhibit-subitemid)) `(,delete-sublist-obj-field))))))))))))))

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
