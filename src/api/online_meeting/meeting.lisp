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

File: src/api/online_meeting/meeting.lisp
Description: /online_meeting/meeting API functions
|#

(in-package :cl-xplan-api/api)

;; online_meeting/meeting - GET /resourceful/online_meeting/meeting and GET /resourceful/online_meeting/meeting/:meeting_id
(define-entrypoint online_meeting/meeting :get
  (meeting_id)
  ((start_time :cond (and (not meeting_id) start_time))
   (end_time :cond (and (not meeting_id) end_time))
   (meeting_role :cond (and (not meeting_id) meeting_role)))
  :resource (format nil "/online_meeting/meeting~@[/~A~]" meeting_id))

;; online_meeting/meeting - POST /resourceful/online_meeting/meeting
(define-entrypoint online_meeting/meeting :post
  () (participant_ids subject organizer_id start_time end_time event_id)
  :resource "/online_meeting/meeting")

;; online_meeting/meeting - PATCH /resourceful/online_meeting/meeting/:meeting_id
(define-entrypoint online_meeting/meeting :patch
  (meeting_id) (participant_ids subject organizer_id start_time end_time event_id)
  :resource (format nil "/online_meeting/meeting/~A" meeting_id))

;; online_meeting/meeting - DELETE /resourceful/online_meeting/meeting/:meeting_id
(define-entrypoint online_meeting/meeting :delete
  (meeting_id) ()
  :resource (format nil "/online_meeting/meeting/~A" meeting_id))

;; online_meeting/meeting - POST /resourceful/online_meeting/meeting/:meeting_id?_method=attend
(define-entrypoint online_meeting/meeting :attend
  (meeting_id) ()
  :documentation "Launch or Join a OnlineMeeting meeting, depending on the session user's role"
  :single-method :post
  :single-parms-as-body T
  :single-resource (format nil "/online_meeting/meeting/~A?_method=attend" meeting_id)
  :bulk-resource (format nil "/online_meeting/meeting/~A" meeting_id))
