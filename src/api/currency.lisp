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

File: src/api/currency.lisp
Description: /currency API functions
|#

(in-package :cl-xplan-api/api)

;;; currency

;; currency - POST /resourceful/currency?_method=convert
(define-entrypoint currency :convert
  ()
  (values src_currency_code dest_currency_code date)
  :single-method :post
  :single-resource "/currency?_method=convert"
  :single-parms-as-body T
  :bulk-resource "/currency"
  :documentation "Convert a vector of values from their source currency to target currency. The default exchange rate is the latest closing price; if date is provided, it will return the the closing price exchange rate of that particular date.")

;;; currency-v2

;; currency-v2 - POST /resourceful/currency-v2?_method=convert
(define-entrypoint currency-v2 :convert
  ()
  (values target_currency_code ((use_cached_exchange_rate nil cache-p) :cond cache-p :value (if use_cached_exchange_rate 1 0)) date)
  :single-method :post
  :single-resource "/currency-v2?_method=convert"
  :single-parms-as-body T
  :bulk-resource "/currency-v2"
  :documentation "Convert a vector of currency values to the target currency. If date is provided, it will return the the closing price exchange rate of that particular date.")

;; currency-v2 - POST /resourceful/currency-v2?_method=sum
(define-entrypoint currency-v2 :sum
  ()
  (values target_currency_code entityid)
  :documentation "Convert a vector of currency values to the target currency and return their sum-up value. If target currency code is not provided, the currency code will be derived from the given entity identified by the entityid or the login session."
  :single-method :post
  :single-resource "/currency-v2?_method=sum"
  :single-parms-as-body T
  :bulk-resource "/currency-v2")
