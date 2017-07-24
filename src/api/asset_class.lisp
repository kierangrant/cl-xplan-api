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

File: src/api/asset_class/asset_class.lisp
Description: /asset_class API functions
|#

(in-package :cl-xplan-api/api)

;; asset_class - GET /resourceful/asset_class and GET /resourceful/asset_class/:xplan_id

(define-entrypoint asset_class :get
  (xplan_id)
  (fields ((include_other nil other-p) :cond other-p :value (if include_other 1 0)))
  :documentation "Retrieve a collection of asset_class resources.
If xplan-id is specified, only that asset class is retrieved and include-other is ignored.
Otherwise, returns a collection, and include-other, if T, \"other\" asset classes will be included."
  :resource (format NIL "/asset_class~@[/~A~]" xplan_id))
