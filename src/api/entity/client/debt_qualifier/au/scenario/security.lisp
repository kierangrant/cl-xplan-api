#|
This file is part of CL-XPLAN-API, the Lisp XPLAN API Library

Copyright (C) 2018 Kieran Grant
This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser General Public License
(http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Lisp Lesser GNU General Public License for more details.

File: src/api/entity/client/debt_qualifier/au/scenario/security.lisp
Description: /entity/client/debt_qualifier/au/scenario/security API functions
|#

(in-package :cl-xplan-api/api)

(define-dynamiclike-entrypoints
    (entity/client/debt_qualifier/au/scenario/security "debt_qualifier/au/scenario" "/entity/client" "security")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field securitiy_id)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (mortgage_type city toal_project_cost legal_description other_note
						 security_zoning new_security security_type security_tenure
						 security_status intention suburb postcode ownership address
						 estimated purchase_price valuation_by_valuer rateable_value))
    :patch-defaults (:default-args (mortgage_type city toal_project_cost legal_description other_note
						  security_zoning new_security security_type security_tenure
						  security_status intention suburb postcode ownership address
						  estimated purchase_price valuation_by_valuer rateable_value))
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints
    (entity/client-v2/debt_qualifier/au/scenario/security "debt_qualifier/au/scenario" "/entity/client-v2"
							  "security")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field securitiy_id)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (mortgage_type city toal_project_cost legal_description other_note
						 security_zoning new_security security_type security_tenure
						 security_status intention suburb postcode ownership address
						 estimated purchase_price valuation_by_valuer rateable_value))
    :patch-defaults (:default-args (mortgage_type city toal_project_cost legal_description other_note
						  security_zoning new_security security_type security_tenure
						  security_status intention suburb postcode ownership address
						  estimated purchase_price valuation_by_valuer rateable_value))
    :put-defaults (:inhibit t))

(define-dynamiclike-entrypoints
    (entity/client-v3/debt_qualifier/au/scenario/security "debt_qualifier/au/scenario" "/entity/client-v3"
							  "security")
    :request-defaults (:list-obj-field scenario_id :sublist-obj-field securitiy_id)
    :get-defaults (:default-args nil)
    :post-defaults (:default-args (mortgage_type city toal_project_cost legal_description other_note
						 security_zoning new_security security_type security_tenure
						 security_status intention suburb postcode ownership address
						 estimated purchase_price valuation_by_valuer rateable_value))
    :patch-defaults (:default-args (mortgage_type city toal_project_cost legal_description other_note
						  security_zoning new_security security_type security_tenure
						  security_status intention suburb postcode ownership address
						  estimated purchase_price valuation_by_valuer rateable_value))
    :put-defaults (:inhibit t))
