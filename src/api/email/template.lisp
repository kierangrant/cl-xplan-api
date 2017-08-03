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

File: src/api/email/template.lisp
Description: /email/template API functions
|#

(in-package :cl-xplan-api/api)

;; GET /resourceful/email/template and GET /resourceful/email/template/:templateid
(define-entrypoint email/template :GET
  (templateid)
  ((entity_type :cond (and (not templateid) entity_type))
   (kind :cond (and (not templateid) kind))
   (page_size :cond (and (not templateid) page_size))
   (page_sort :cond (and (not templateid) page_sort))
   (page_bookmark :cond (and (not templateid) page_bookmark))
   (page_dir :cond (and (not templateid) page_dir)))
  :documentation "Collection of Email Templates visible to the current user if templateid ommited otherwise just the particular resource"
  :resource (format nil "/email/template~@[/~A~]" templateid))
