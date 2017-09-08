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

File: src/api/news.lisp
Description: /news API functions
|#

(in-package :cl-xplan-api/api)

;; news - GET /resourceful/news
(define-entrypoint news :get
  ()
  (((only_subscribed nil only_subscribed-p) :cond only_subscribed-p :value (if only_subscribed 1 0))
   from_date to_date channel_ids limit)
  :resource "/news"
  :documentation "Read-only API for a list of current news items")
