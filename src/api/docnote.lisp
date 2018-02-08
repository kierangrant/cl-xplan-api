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
  (docid created_date.start_date created_date.end_date modified_date.start_date
	 modified_date.end_date reference_date.start_date reference_date.end_date
	 firstread.entity_id (firstread.include_read_notes nil read-notes-p)
	 firstread.read_entity_roles firstread.max_num_entities)
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
   ((created_date nil created_date-p) :cond (not docid)
    :value
    (if created_date-p created_date
	(cond-hash
	  (created_date.start_date "start_date")
	  (created_date.end_date "end_date"))))
   ((modified_date nil modified_date-p) :cond (not docid)
    :value
    (if modified_date-p modified_date
	(cond-hash
	  (modified_date.start_date "start_date")
	  (modified_date.end_date "end_date"))))
   ((reference_date nil reference_date-p) :cond (not docid)
    :value
    (if reference_date-p reference_date
	(cond-hash
	  (reference_date.start_date "start_date")
	  (reference_date.end_date "end_date"))))
   ((session_info nil sinfo-p) :cond (and (not docid) sinfo-p) :value (if session_info 1 0))
   ((firstread nil firstread-p) :cond (not docid)
    :value
    (if firstread-p firstread
	(cond-hash
	  (firstread.entity_id "entity_id")
	  (read-notes-p "include_read_notes" firstread.include_read_notes)
	  (firstread.read_entity_roles "read_entity_roles")
	  (firstread.max_num_entities "max_num_entities"))))
   (page_size :cond (and (not docid) page_size))
   (page_sort :cond (and (not docid) page_sort))
   (page_bookmark :cond (and (not docid) page_bookmark))
   (page_dir :cond (and (not docid) page_dir))
   (account_group_codes :cond (and (not docid) account_group_codes))
   (accounts :cond (and (not docid) accounts)))
  :documentation "created_date, modified_date, reference_date and firstread are 'objects'. They need to be either an alist or hashtable specifing their contents as per API."
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
  (docid created_date.start_date created_date.end_date modified_date.start_date modified_date.end_date
	 reference_date.start_date reference_date.end_date firstread.entity_id
	 (firstread.include_read_notes nil include-read-p) firstread.read_entity_roles
	 firstread.max_num_entities)
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
   ((created_date nil created_date-p) :cond (not docid)
    :value
    (if created_date-p created_date
	(cond-hash
	  (created_date.start_date "start_date")
	  (created_date.end_date "end_date"))))
   ((modified_date nil modified_date-p) :cond (not docid)
    :value
    (if modified_date-p modified_date
	(cond-hash
	  (modified_date.start_date "start_date")
	  (modified_date.end_date "end_date"))))
   ((reference_date nil reference_date-p) :cond (not docid)
    :value
    (if reference_date-p reference_date
	(cond-hash
	  (reference_date.start_date "start_date")
	  (reference_date.end_date "end_date"))))
   ((session_info nil sinfo-p) :cond (and (not docid) sinfo-p) :value (if session_info 1 0))
   ((firstread nil firstread-p) :cond (not docid)
    :value
    (if firstread-p firstread
	(cond-hash
	  (firstread.entity_id "entity_id")
	  (include-read-p "include_read_notes" (if firstread.include_read_notes 1 0))
	  (firstread.read_entity_roles "read_entity_roles")
	  (firstread.max_num_entities "max_num_entities"))))
   (doctype_exclude :cond (and (not docid) doctype_exclude))
   (docsubtype_exclude :cond (and (not docid) docsubtype_exclude))
   ((has_digital_signature nil has_digital_signature-p) :cond (and (not docid) has_digital_signature-p)
    :value (if has_digital_signature 1 0))
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
(define-entrypoint docnote-v2 :delete
  (docid) ()
  :resource (format nil "/docnote-v2/~A" docid))
