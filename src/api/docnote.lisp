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

File: src/api/docnote.lisp
Description: /docnote API Functions
|#

(in-package :cl-xplan-api/api)

;;; docnote

;; docnote - GET /resourceful/docnote and GET /resourceful/docnote/:docid
(define-entrypoint docnote :get
  (docid)
  ((entity_ids :cond (and (not docid) entity_ids))
   (subject :cond (and (not docid) subject))
   (created_by :cond (and (not docid) created_by))
   ((has_metadata nil metadata-p) :cond (and (not docid) metadata-p) :value (if has_metadata 1 0))
   (categories :cond (and (not docid) categories))
   (link_thread :cond (and (not docid) link_thread))
   (doctype :cond (and (not docid) doctype))
   (docsubtype :cond (and (not docid) docsubtype))
   (modified_by :cond (and (not docid) modified_by))
   (keywords :cond (and (not docid) keywords))
   (created_date.start_date :cond (and (not docid) created_date.start_date))
   (created_date.end_date :cond (and (not docid) created_date.end_date))
   (modified_date.start_date :cond (and (not docid) modified_date.start_date))
   (modified_date.end_date :cond (and (not docid) modified_date.end_date))
   (reference_date.start_date :cond (and (not docid) reference_date.start_date))
   (reference_date.end_date :cond (and (not docid) reference_date.end_date))
   ((session_info nil sinfo-p) :cond (and (not docid) sinfo-p) :value (if session_info 1 0))
   (firstread.entity_id :cond (and (not docid) firstread.entity_id))
   ((firstread.include_read_notes nil include-read-p) :cond (and (not docid) include-read-p)
    :value (if firstread.include_read_notes 1 0))
   (page_size :cond (and (not docid) page_size))
   (page_sort :cond (and (not docid) page_sort))
   (page_bookmark :cond (and (not docid) page_bookmark))
   (page_dir :cond (and (not docid) page_dir))
   (account_group_codes :cond (and (not docid) account_group_codes))
   (accounts :cond (and (not docid) accounts)))
  :resource (format nil "/docnote~@[/~A~]" docid))

;; docnote - POST /resourceful/docnote
(define-entrypoint docnote :post
  ()
  (body mimetype subject metadata
	((is_deleted nil deleted-p) :cond deleted-p :value (if is_deleted 1 0))
	reference_date entity_ids
	((is_locked nil locked-p) :cond locked-p :value (if is_locked 1 0))
	permission shared_entity_ids
	((is_client_access_accessible nil client-access-p) :cond client-access-p
	 :value (if is_client_access_accessible 1 0))
	((is_client_share_all nil client-share-p) :cond client-share-p
	 :value (if is_client_share_all 1 0))
	((is_referrer_accessible nil referrer-access-p) :cond referrer-access-p
	 :value (if is_referrer_accessible 1 0))
	((is_profadviser_accessible nil profadviser-access-p) :cond profadviser-access-p
	 :value (if is_profadviser_accessible 1 0))
	edit_privilege
	((is_published nil published-p) :cond published-p :value (if is_published 1 0))
	summary guid
	((is_unallocated nil unallocated-p) :cond unallocated-p :value (if is_unallocated 1 0))
	categories
	accounts
	account_group_codes
	doctype
	docsubtype
	actual_creator)
  :resource "/docnote")

;; docnote - PATCH /resourceful/docnote/:docid
(define-entrypoint docnote :patch
  (docid)
  (body mimetype subject metadata
	((is_deleted nil deleted-p) :cond deleted-p :value (if is_deleted 1 0))
	reference_date entity_ids
	((is_locked nil locked-p) :cond locked-p :value (if is_locked 1 0))
	permission shared_entity_ids
	((is_client_access_accessible nil client-access-p) :cond client-access-p
	 :value (if is_client_access_accessible 1 0))
	((is_client_share_all nil client-share-p) :cond client-share-p
	 :value (if is_client_share_all 1 0))
	((is_referrer_accessible nil referrer-access-p) :cond referrer-access-p
	 :value (if is_referrer_accessible 1 0))
	((is_profadviser_accessible nil profadviser-access-p) :cond profadviser-access-p
	 :value (if is_profadviser_accessible 1 0))
	edit_privilege
	((is_published nil published-p) :cond published-p :value (if is_published 1 0))
	summary guid
	((is_unallocated nil unallocated-p) :cond unallocated-p :value (if is_unallocated 1 0))
	categories
	accounts
	account_group_codes
	doctype
	docsubtype)
  :resource (format nil "/docnote/~A" docid))

;; docnote - DELETE /resourceful/docnote/:docid
(define-entrypoint docnote :delete (docid) () :resource (format nil "/docnote/~A" docid))

;;; docnote-v2

;; docnote-v2 - GET /resourceful/docnote-v2 and GET /resourceful/docnote-v2/:docid
(define-entrypoint docnote-v2 :get
  (docid)
  ((entity_ids :cond (and (not docid) entity_ids))
   (subject :cond (and (not docid) subject))
   (created_by :cond (and (not docid) created_by))
   ((has_metadata nil metadata-p) :cond (and (not docid) metadata-p) :value (if has_metadata 1 0))
   (categories :cond (and (not docid) categories))
   (link_thread :cond (and (not docid) link_thread))
   (doctype :cond (and (not docid) doctype))
   (docsubtype :cond (and (not docid) docsubtype))
   (modified_by :cond (and (not docid) modified_by))
   (keywords :cond (and (not docid) keywords))
   (created_date.start_date :cond (and (not docid) created_date.start_date))
   (created_date.end_date :cond (and (not docid) created_date.end_date))
   (modified_date.start_date :cond (and (not docid) modified_date.start_date))
   (modified_date.end_date :cond (and (not docid) modified_date.end_date))
   (reference_date.start_date :cond (and (not docid) reference_date.start_date))
   (reference_date.end_date :cond (and (not docid) reference_date.end_date))
   ((session_info nil sinfo-p) :cond (and (not docid) sinfo-p) :value (if session_info 1 0))
   (firstread.entity_id :cond (and (not docid) firstread.entity_id))
   ((firstread.include_read_notes nil include-read-p) :cond (and (not include-read-p))
    :value (if firstread.include_read_notes 1 0))
   (page_size :cond (and (not docid) page_size))
   (page_sort :cond (and (not docid) page_sort))
   (page_bookmark :cond (and (not docid) page_bookmark))
   (page_dir :cond (and (not docid) page_dir))
   (account_group_codes :cond (and (not docid) account_group_codes))
   (accounts :cond (and (not docid) accounts)))
  :resource (format nil "/docnote-v2~@[/~A~]" docid))

;; docnote-v2 - POST /resourceful/docnote-v2
(define-entrypoint docnote-v2 :post
  ()
  (body mimetype subject metadata
	((is_deleted nil deleted-p) :cond deleted-p :value (if is_deleted 1 0))
	reference_date
	entity_ids
	((is_locked nil locked-p) :cond locked-p :value (if is_locked 1 0))
	permission shared_entity_ids
	((is_client_access_accessible nil client-access-p) :cond client-access-p
	 :value (if is_client_access_accessible 1 0))
	((is_client_share_all nil client-share-p) :cond client-share-p :value (if is_client_share_all 1 0))
	((is_referrer_accessible nil referrer-access-p) :cond referrer-access-p
	 :value (if is_referrer_accessible 1 0))
	((is_profadviser_accessible nil profadviser-access-p) :cond profadviser-access-p
	 :value (if is_profadviser_accessible 1 0))
	edit_privilege
	((is_published nil published-p) :cond published-p :value (if is_published 1 0))
	summary guid
	((is_unallocated nil unallocated-p) :cond unallocated-p :value (if is_unallocated 1 0))
	categories accounts account_group_codes doctype docsubtype actual_creator)
  :resource "/docnote-v2")

;; docnote-v2 - PATCH /resourceful/docnote-v2/:docid
(define-entrypoint docnote-v2 :patch
  ()
  (body mimetype subject metadata
	((is_deleted nil deleted-p) :cond deleted-p :value (if is_deleted 1 0))
	reference_date
	entity_ids
	((is_locked nil locked-p) :cond locked-p :value (if is_locked 1 0))
	permission shared_entity_ids
	((is_client_access_accessible nil client-access-p) :cond client-access-p
	 :value (if is_client_access_accessible 1 0))
	((is_client_share_all nil client-share-p) :cond client-share-p :value (if is_client_share_all 1 0))
	((is_referrer_accessible nil referrer-access-p) :cond referrer-access-p
	 :value (if is_referrer_accessible 1 0))
	((is_profadviser_accessible nil profadviser-access-p) :cond profadviser-access-p
	 :value (if is_profadviser_accessible 1 0))
	edit_privilege
	((is_published nil published-p) :cond published-p :value (if is_published 1 0))
	summary guid
	((is_unallocated nil unallocated-p) :cond unallocated-p :value (if is_unallocated 1 0))
	categories accounts account_group_codes doctype docsubtype)
  :resource "/docnote-v2")

;; docnote-v2 - DELETE /resourceful/docnote-v2/:docid
(define-entrypoint docnote-v2 :delete (docid) () :resource (format nil "/docnote-v2/~A" docid))

;;; docnote/attachment

;; docnote/attachment - GET /resourceful/docnote/:docid/attachment and GET /resourceful/docnote/:docid/attachment/:docpartid
(define-entrypoint docnote/attachment :get (docid docpartid) ()
		   :resource (format nil "/docnote/~A/attachment~@[/~A~]" docid docpartid))

;; docnote/attachment - POST /resourceful/docnote/:docid/attachment
(define-entrypoint docnote/attachment :post (docid) (filename mimetype metadata content)
		   :resource (format nil "/docnote/~A/attachment" docid))

;; docnote/attachment - PATCH /resourceful/docnote/:docid/attachment/:docpartid
(define-entrypoint docnote/attachment :patch (docid docpartid) (filename mimetype metadata content)
		   :resource (format nil "/docnote/~A/attachment/~A" docid docpartid))

;; docnote/attachment - DELETE /resourceful/docnote/:docid/attachment/:docpartid
(define-entrypoint docnote/attachment :delete (docid docpartid) ()
		   :resource (format nil "/docnote/~A/attachment/~A" docid docpartid))
;;; docnote-v2/attachment

;; docnote-v2/attachment - GET /resourceful/docnote-v2/:docid/attachment and GET /resourceful/docnote-v2/:docid/attachment/:docpartid
(define-entrypoint docnote-v2/attachment :get (docid docpartid) ()
		   :resource (format nil "/docnote-v2/~A/attachment~@[/~A~]" docid docpartid))

;; docnote-v2/attachment - POST /resourceful/docnote-v2/:docid/attachment
(define-entrypoint docnote-v2/attachment :post (docid) (filename mimetype metadata content)
		   :resource (format nil "/docnote-v2/~A/attachment" docid))

;; docnote-v2/attachment - PATCH /resourceful/docnote-v2/:docid/attachment/:docpartid
(define-entrypoint docnote-v2/attachment :patch (docid docpartid) (filename mimetype metadata content)
		   :resource (format nil "/docnote-v2/~A/attachment/~A" docid docpartid))

;; docnote-v2/attachment - DELETE /resourceful/docnote-v2/:docid/attachment/:docpartid
(define-entrypoint docnote-v2/attachment :delete (docid docpartid) ()
		   :resource (format nil "/docnote-v2/~A/attachment/~A" docid docpartid))

;;; docnote/attachment-v2

;; docnote/attachment-v2 - GET /resourceful/docnote/:docid/attachment-v2 and GET /resourceful/docnote/:docid/attachment-v2/:docpartid
(define-entrypoint docnote/attachment-v2 :get (docid docpartid) ()
		   :resource (format nil "/docnote/~A/attachment-v2~@[/~A~]" docid docpartid))

;; docnote/attachment-v2 - POST /resourceful/docnote/:docid/attachment-v2
(define-entrypoint docnote/attachment-v2 :post (docid) (filename mimetype metadata content)
		   :resource (format nil "/docnote/~A/attachment-v2" docid))

;; docnote/attachment-v2 - PATCH /resourceful/docnote/:docid/attachment-v2/:docpartid
(define-entrypoint docnote/attachment-v2 :patch (docid docpartid) (filename mimetype metadata content)
		   :resource (format nil "/docnote/~A/attachment-v2/~A" docid docpartid))

;; docnote/attachment-v2 - DELETE /resourceful/docnote/:docid/attachment-v2/:docpartid
(define-entrypoint docnote/attachment-v2 :delete (docid docpartid) ()
		   :resource (format nil "/docnote/~A/attachment-v2/~A" docid docpartid))

;;; docnote-v2/attachment-v2

;; docnote-v2/attachment-v2 - GET /resourceful/docnote-v2/:docid/attachment-v2 and GET /resourceful/docnote-v2/:docid/attachment-v2/:docpartid
(define-entrypoint docnote-v2/attachment-v2 :get (docid docpartid) ()
		   :resource (format nil "/docnote-v2/~A/attachment-v2~@[/~A~]" docid docpartid))

;; docnote-v2/attachment-v2 - PATCH /resourceful/docnote-v2/:docid/attachment-v2/:docpartid
(define-entrypoint docnote-v2/attachment-v2 :patch (docid docpartid)
		   (filename mimetype metadata content)
		   :resource (format nil "/docnote-v2/~A/attachment-v2/~A" docid docpartid))

;; docnote-v2/attachment-v2 - DELETE /resourceful/docnote-v2/:docid/attachment-v2/:docpartid
(define-entrypoint docnote-v2/attachment-v2 :delete (docid docpartid) ()
		   :resource (format nil "/docnote-v2/~A/attachment-v2/~A" docid docpartid))

;;; docnote/attachment-v3

;; docnote/attachment-v3 - GET /resourceful/docnote/:docid/attachment-v3 and GET /resourceful/docnote/:docid/attachment-v3/:docpartid
(define-entrypoint docnote/attachment-v3 :get (docid docpartid) ()
		   :resource (format nil "/docnote/~A/attachment-v3~@[/~A~]" docid docpartid))

;; docnote/attachment-v3 - POST /resourceful/docnote/:docid/attachment-v3
(define-entrypoint docnote/attachment-v3 :post (docid) (filename mimetype metadata content)
		   :resource (format nil "/docnote/~A/attachment-v3" docid))

;; docnote/attachment-v3 - PATCH /resourceful/docnote/:docid/attachment-v3/:docpartid
(define-entrypoint docnote/attachment-v3 :patch (docid docpartid) (filename mimetype metadata content)
		   :resource (format nil "/docnote/~A/attachment-v3/~A" docid docpartid))

;; docnote/attachment-v3 - DELETE /resourceful/docnote/:docid/attachment-v3/:docpartid
(define-entrypoint docnote/attachment-v3 :delete (docid docpartid) ()
		   :resource (format nil "/docnote/~A/attachment-v3/~A" docid docpartid))

;;; docnote-v2/attachment-v3

;; docnote-v2/attachment-v3 - GET /resourceful/docnote-v2/:docid/attachment-v3 and GET /resourceful/docnote-v2/:docid/attachment-v3/:docpartid
(define-entrypoint docnote-v2/attachment-v3 :get (docid docpartid) ()
		   :resource (format nil "/docnote-v2/~A/attachment-v3~@[/~A~]" docid docpartid))

;; docnote-v2/attachment-v3 - POST /resourceful/docnote-v2/:docid/attachment-v3
(define-entrypoint docnote-v2/attachment-v3 :post (docid) (filename mimetype metadata content)
		   :resource (format nil "/docnote-v2/~A/attachment-v3" docid))

;; docnote-v2/attachment-v3 - PATCH /resourceful/docnote-v2/:docid/attachment-v3/:docpartid
(define-entrypoint docnote-v2/attachment-v3 :patch (docid docpartid)
		   (filename mimetype metadata content)
		   :resource (format nil "/docnote-v2/~A/attachment-v3/~A" docid docpartid))

;; docnote-v2/attachment-v3 - DELETE /resourceful/docnote-v2/:docid/attachment-v3/:docpartid
(define-entrypoint docnote-v2/attachment-v3 :delete (docid docpartid) ()
		   :resource (format nil "/docnote-v2/~A/attachment-v3/~A" docid docpartid))

;;; docnote/attachment-v3/content

;; docnote/attachment-v3/content - GET /resourceful/docnote/:docid/attachment-v3/:docpartid/content
(define-entrypoint docnote/attachment-v3/content :get (docid docpartid) ()
		   :resource (format nil "/docnote/~A/attachment-v3/~A/content" docid docpartid))

;; docnote-v2/attachment-v3/content - GET /resourceful/docnote-v2/:docid/attachment-v3/:docpartid/content
(define-entrypoint docnote-v2/attachment-v3/content :get (docid docpartid) ()
		   :resource (format nil "/docnote-v2/~A/attachment-v3/~A/content" docid docpartid))

;;; docnote-v2/body

;; docnote-v2/body - GET /resourceful/docnote-v2/:docid/body
(define-entrypoint docnote-v2/body :get (docid) () :resource (format nil "/docnote-v2/~A/body" docid))

;;; docnote-v2/case

;; docnote-v2/case
(define-entrypoint docnote-v2/case :get (docid) () :resource (format nil "/docnote-v2/~A/caes" docid))

;;; docnote/category

;; docnote/category - GET /resourceful/docnote/category and GET /resourceful/docnote/category/:catid
(define-entrypoint docnote/category :get (catid) ((parentid :cond (and (not catid) parentid)))
		   :resource (format nil "/docnote/category~@[/~A~]" catid))

;; docnote/category - POST /resourceful/docnote/category
(define-entrypoint docnote/category :post
  ()
  (parentid referrer_permission edit_privilege feed_permission
	    ((is_publishable nil publishable-p) :cond publishable-p :value (if is_publishable 1 0))
	    name permission)
  :resource "/docnote/category")

;; docnote/category - PATCH /resourceful/docnote/category/:catid
(define-entrypoint docnote/category :patch
  (catid)
  (parentid referrer_permission edit_privilege feed_permission
	    ((is_publishable nil publishable-p) :cond publishable-p :value (if is_publishable 1 0))
	    name permission)
  :resource (format nil "/docnote/category/~A" catid))

;; docnote/category - DELETE /resourceful/docnote/category/:catid
(define-entrypoint docnote/category :delete (catid) ()
		   :resource (format nil "/docnote/category/~A" catid))

;;; docnote-v2/category

;; docnote-v2/category - GET /resourceful/docnote-v2/category and GET /resourceful/docnote-v2/category/:catid
(define-entrypoint docnote-v2/category :get (catid) ((parentid :cond (and (not catid) parentid)))
		   :resource (format nil "/docnote-v2/category~@[/~A~]" catid))

;; docnote-v2/category - POST /resourceful/docnote-v2/category
(define-entrypoint docnote-v2/category :post
  ()
  (parentid referrer_permission edit_privilege feed_permission
	    ((is_publishable nil publishable-p) :cond publishable-p :value (if is_publishable 1 0))
	    name permission)
  :resource "/docnote-v2/category")

;; docnote-v2/category - PATCH /resourceful/docnote-v2/category/:catid
(define-entrypoint docnote-v2/category :patch
  (catid)
  (parentid referrer_permission edit_privilege feed_permission
	    ((is_publishable nil publishable-p) :cond publishable-p :value (if is_publishable 1 0))
	    name permission)
  :resource (format nil "/docnote-v2/category/~A" catid))

;; docnote-v2/category - DELETE /resourceful/docnote-v2/category/:catid
(define-entrypoint docnote-v2/category :delete (catid) ()
		   :resource (format nil "/docnote-v2/category/~A" catid))

;;; docnote-v2/email

;; docnote-v2/email - POST /resourceful/docnote-v2/:docid/email?_method=send
(define-entrypoint docnote-v2/email :send (docid) (email_address template)
		   :single-resource (format nil "/docnote-v2/~A/email" docid)
		   :single-method :post
		   :hidden-single-parameters (("_method" . "send"))
		   :bulk-resource (format nil "/docnote-v2/~A/email?_method=send" docid)
		   :bulk-method :post
		   :documentation "Send an email based on a specified email template. Attachments on the note will be sent as email attachments")

;;; docnote-v2/firstread

;; docnote-v2/firstread - GET /resourceful/docnote-v2/:docid/firstread and GET /resourceful/docnote-v2/:docid/firstread/:entityid
(define-entrypoint docnote-v2/firstread :get (docid entityid) ()
		   :resource (format nil "/docnote-v2/~A/firstread~@[/~A~]" docid entityid))

;; docnote-v2/firstread - POST /resourceful/docnote-v2/:docid/firstread
(define-entrypoint docnote-v2/firstread :post (docid) ()
		   :resource (format nil "/docnote-v2/~A/firstread" docid))
