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

File: src/api/online_meeting/meeting/attendee.lisp
Description: /online_meeting/meeting/attendee API functions
|#

(in-package :cl-xplan-api/api)

;; online_meeting/meeting/attendee - GET /resourceful/online_meeting/meeting/:meeting_id/attendee
(define-entrypoint online_meeting/meeting/attendee :get
  (meeting_id) ()
  :resource (format nil "/online_meeting/meeting/~A/attendee" meeting_id))
