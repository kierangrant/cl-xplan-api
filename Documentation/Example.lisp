(defpackage :test (:use :cl :cl-xplan-api))
(in-package :test)

(defvar *session*
  (make-instance
   'xplan-session
   :username "USERNAME"
   :password "PASSWORD"
   :base-url "https://an-xplan-api-server.com"
   :api-key "YOUR-API-KEY"))

;; We will use with-xplan-session macro, using existing session object
;; we will close session after use.
(with-xplan-session (sess *session*)
  ;; let's do a bulk request
  (with-bulk-request (sess req)
    ;; first request is GET /entity/client-v3/:entity_id
    (entity/client-v3
     req
     :get
     :request-name "client"
     :fields #("first_name" "last_name" "client_adviser")
     :entity_id entity-id)
    ;; second request is GET /entity/user-v2/:entity_id
    ;; Notice in a bulk request we can use JSONPath to use results from a previous request
    ;; the 'client' just after the '=' is the name of the request we are pulling data from
    (entity/user-v2
     req
     :get
     :request-name "adviser"
     :fields #("first_name" "last_name")
     :entity_id "{result=client:$.client_adviser}")
    ;; For a bulk-request, we must call process-request when we are finished setting it up.
    ;; As with a normal request, if the server returns an error, we throw a non-continuable error.
    ;; But if a subrequest has an error, we throw a continuable error, as XPlan documentation says the
    ;; whole request can still be deemed sucessful.
    (process-request req)
    (let ((client (response (get-request-by-name req "client")))
	  (adviser (response (get-request-by-name req "adviser"))))
      `(:client
	(:first-name ,(gethash "first_name" client) :last-name ,(gethash "last_name" client))
	:adviser
	(:first-name ,(gethash "first_name" adviser) :last-name ,(gethash "last_name" adviser))))))
-->
(:CLIENT
 (:FIRST-NAME "Test" :LAST-NAME "KTestclient")
 :ADVISER
 (:FIRST-NAME "Kieran" :LAST-NAME "Grant"))
