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

File: src/api/debt_qualifier/gb/custom_panel.lisp
Description: /debt_qualifier/gb/custom_panel API Functions
|#

(in-package :cl-xplan-api/api)

;; debt_qualifier/gb/custom_panel - GET /resourceful/debt_qualifier/gb/custom_panel/:entity_id and GET /resourceful/debt_qualifier/gb/custom_panel/:entity_id/:panel_id
(define-entrypoint debt_qualifier/gb/custom_panel :get
  (entity_id panel_id) ((panel_group :cond (and (not panel_id) panel_group)))
  :resource (format nil "GET /resourceful/debt_qualifier/gb/custom_panel/~A~@[/~A~]" entity_id panel_id))

;; debt_qualifier/gb/custom_panel - POST /resourceful/debt_qualifier/gb/custom_panel/:entity_id
(define-entrypoint debt_qualifier/gb/custom_panel :get
  (entity_id) (panel_name panel_group lender_ids packager_ids)
  :resource (format nil "/debt_qualifier/gb/custom_panel/~A" entity_id))

;; debt_qualifier/gb/custom_panel - PATCH /resourceful/debt_qualifier/gb/custom_panel/:entity_id/:panel_id
(define-entrypoint debt_qualifier/gb/custom_panel :patch (entity_id panel_id)
		   (panel_name panel_group lender_ids packager_ids)
		   :resource (format nil "/debt_qualifier/gb/custom_panel/~A/~A" entity_id panel_id))

;; debt_qualifier/gb/custom_panel - DELETE /resourceful/debt_qualifier/gb/custom_panel/:entity_id/:panel_id
(define-entrypoint debt_qualifier/gb/custom_panel :delete (entity_id panel_id) ()
		   :resource (format nil "/debt_qualifier/gb/custom_panel/~A/~A" entity_id panel_id))
