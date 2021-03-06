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

File: src/api/job_queue/completed_job.lisp
Description: /job_queue/completed_job API functions
|#

(in-package :cl-xplan-api/api)

;; job_queue/completed_job - GET /resourceful/job_queue/completed_job/:id
(define-entrypoint job_queue/completed_job :get
  (id) ()
  :documentation "Access the results of completed jobs"
  :resource (format nil "/job_queue/completed_job/~A" id))
