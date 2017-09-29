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

File: src/api/payment_gateway/worldpay/payment.lisp
Description: /payment_gateway/worldpay/payment API functions
|#

(in-package :cl-xplan-api/api)

;; payment_gateway/worldpay/payment - PUT /resourceful/payment_gateway/worldpay/payment
(define-entrypoint payment_gateway/worldpay/payment :put
  (billing_address.address1 billing_address.address2 billing_address.address3 billing_address.city
			    billing_address.state billing_address.postal_code billing_address.country_code
			    billing_address.telephone_number)
  (amount currency_code order_token order_description redirect_url payment_gateway_id name customer_order_code
	  settlement_currency ((3ds_order nil 3ds_order-p) :cond 3ds_order-p :value (if 3ds_order 1 0))
	  shopper_accept_header shopper_user_agent shopper_session_id shopper_ip_address
	  (billing_address
	   :cond (or billing_address billing_address.address1 billing_address.address2
		     billing_address.address3 billing_address.city billing_address.state
		     billing_address.postal_code billing_address.country_code billing_address.telephone_number)
	   :value
	   (if billing_address billing_address
	       (cond-hash
		 (billing_address.address1 "address1")
		 (billing_address.address2 "address2")
		 (billing_address.address3 "address3")
		 (billing_address.city "city")
		 (billing_address.state "state")
		 (billing_address.postal_code "postal_code")
		 (billing_address.country_code "country_code")
		 (billing_address.telephone_number "telephone_number")))))
  :resource "payment_gateway/worldpay/payment"
  :documentation "Send a payment order to the worldpay gateway and return the payment status."
  :single-parms-as-body T)

;; payment_gateway/worldpay/payment - GET /resourceful/payment_gateway/worldpay/payment/:order_code
(define-entrypoint payment_gateway/worldpay/payment :get
  (order_code) ()
  :documentation "Get the status of a pending order."
  :resource (format nil "/payment_gateway/worldpay/payment/~A" order_code))
