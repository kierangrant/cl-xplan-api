CL-XPAN-API is a LISP library that makes it easier to interact with IRESS XPLAN API.

Whilst this software is released under the LLGPL, to actually connect to an IRESS XPLAN API Server requires you to have an API Key from them.

You need a use agreement with IRESS, though, nothing stops you using this library with a server that duplicates their API.

Note: As of 04/07/2017 you need to patch JSON with json-patch.diff, alternatively you can load json-patch.lisp, after loading CL-JSON manually.

Documentation for XPLAN API is avaliable (after registration) at:
https://insights.iressconnect.com/docs/DOC-7376 - Getting Started with XPLAN API
https://insights.iressconnect.com/docs/DOC-7377 - XPLAN API Transaction
https://insights.iressconnect.com/docs/DOC-7378#jive_content_id_Specifying_Request_Dependencies - XPLAN API Batched Requests

Example usage of API:
(defpackage :test (:use :cl :cl-xplan-api))
(in-package :test)

(with-xplan-session (sess nil nil :username "USERNAME" :password "PASSWORD" :base-url "https://an-xplan-api-server.com" :api-key "YOUR-API-KEY")
  (with-bulk-request (sess req)
    (entity/client-v3
     req
     :get
     :request-name "client"
     :fields #("first_name" "last_name" "client_adviser")
     :entity_id entity-id)
    (entity/user-v2
     req
     :get
     :request-name "adviser"
     :fields #("first_name" "last_name")
     :entity_id "{result=client:$.client_adviser}")
    (process-request req)
    (let ((client (response (get-request-by-name req "client")))
	  (adviser (response (get-request-by-name req "adviser"))))
      `(:client
	(:first-name ,(gethash "first_name" client) :last-name ,(gethash "last_name" client))
	:adviser
	(:first-name ,(gethash "first_name" adviser) :last-name ,(gethash "last_name" adviser))))))
-->
(:CLIENT (:FIRST-NAME "Test" :LAST-NAME "KTestclient") :ADVISER
 (:FIRST-NAME "Kieran" :LAST-NAME "Grant"))