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

File: src/api/event.lisp
Description: /event API functions
|#

(in-package :cl-xplan-api/api)

;; GET /resourceful/event and GET /resourceful/event/:eventid
(define-entrypoint event :get
  (eventid)
  ((clients :cond (and (not eventid) clients))
   (users :cond (and (not eventid) users))
   (start :cond (and (not eventid) start))
   (end :cond (and (not eventid) end))
   (timezone :cond (and (not eventid) timezone))
   (link_thread :cond (and (not eventid) link_thread))
   (page :cond (and (not eventid) page))
   (fields :cond (and eventid fields)))
  :resource (format nil "/event~@[/~A~]" eventid))

;; POST /resourceful/event
(define-entrypoint event :post
  ()
  (((isAllDayEvent nil isAllDayEvent-p) :string "isAllDayEvent" :cond isAllDayEvent-p
    :value (if isAllDayEvent 1 0))
   ((isOccurrence nil isOccurrence-p) :string "isOccurrence" :cond isOccurrence-p
    :value (if isOccurrence 1 0))
   ((isRecurring nil isRecurring-p) :string "isRecurring" :cond isRecurring-p :value (if isRecurring 1 0))
   ((client_accessible nil client_accessible-p) :cond client_accessible-p :value (if client_accessible 1 0))
   from_template title event_start event_end description clients users organiser locations locations_plain)
  :resource "/event")

;; PATCH /resourceful/event/:eventid
(define-entrypoint event :patch
  (eventid)
  (((isAllDayEvent nil isAllDayEvent-p) :string "isAllDayEvent" :cond isAllDayEvent-p
    :value (if isAllDayEvent 1 0))
   ((isOccurrence nil isOccurrence-p) :string "isOccurrence" :cond isOccurrence-p
    :value (if isOccurrence 1 0))
   ((isRecurring nil isRecurring-p) :string "isRecurring" :cond isRecurring-p :value (if isRecurring 1 0))
   ((client_accessible nil client_accessible-p) :cond client_accessible-p :value (if client_accessible 1 0))
   from_template title event_start event_end description clients users category organiser locations locations_plain)
  :resource (format nil "/event/~A" eventid))

;; DELETE /resourceful/event/:eventid
(define-entrypoint event :delete (eventid) () :resource (format nil "/event/~A" eventid))

;; GET /resourceful/event-v2
(define-entrypoint event-v2 :get
  ()
  (clients users start end timezone link_thread page_size page_sort page_bookmark page_dir)
  :documentation "Collection of diary events visible to the current user version 2. Allows sorting and pagination."
  :resource "/event-v2")

;; POST /resourceful/event-v2
(define-entrypoint event-v2 :post
  ()
  (((isAllDayEvent nil isAllDayEvent-p) :string "isAllDayEvent" :cond isAllDayEvent-p
    :value (if isAllDayEvent 1 0))
   ((isOccurrence nil isOccurrence-p) :string "isOccurrence" :cond isOccurrence-p
    :value (if isOccurrence 1 0))
   ((isRecurring nil isRecurring-p) :string "isRecurring" :cond isRecurring-p :value (if isRecurring 1 0))
   ((client_accessible nil client_accessible-p) :cond client_accessible-p :value (if client_accessible 1 0))
   from_template title event_start event_end description clients users category organiser locations locations_plain)
  :resource "/event-v2")
