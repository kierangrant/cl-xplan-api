Access Point
------------

The access point for the API is at:
https://my_xplan_site/resourceful

You can see the documentation for the API by visiting that URL (when logged into XPLAN) in a web browser

All responses that have content are returned in JSON format, so you must supply an Accept header indicating you support application/json

Concurrent Sessions
-------------------

By default XPLAN API is configured for single-session-per-XPLAN user, if you need concurrent connections, contact IRESS.

HTTP Methods
------------
- GET - Make a GET request to retrieve data. The data can be an object or a collection.
- PATCH - Make a PATCH request to perform a (partial) update to a resource. With PATCH requests, you only need to provide the data you want to change.
- PUT - Use a PUT request to create or fully replace a resource. This method may only be used for creation when the full URL of the object is known.
- POST - Use a POST request for any other operation on a resource, where the request parameters describe the operation. Or to add a new resource to a collection.

Some resources support operations invoked by sending a POST request and passing a _method parameter containing a string specifying the action’s name. The API documentation for a resource indicates how to call these methods when it applies.

- DELETE - Make a DELETE request to remove a resource.
- OPTIONS - Use an OPTIONS request to retrieve documentation for a resource. you need to specify Content-Type: text/html in the request header to retrieve HTML documentation.

Parameters
==========

Path Parameters
---------------

Some Parameters are passed as part of the request path, for example, entity ID's are usually passed like so:
`GET my_xplan_site/resourceful/docnote-v2/{docid}/attachment-v3/{docpartid}/content`
The {docid} and {docpartid} would be substituted for the actual ID's like so:
`GET my_xplan_site/resourceful/docnote-v2/1234/attachment-v3/5678901234/content`

Query String Parameters
-----------------------

Usually parameters are passed in the URL as Query String Parameters, please see "Types" Documentation
Otherwise, they are passed in the Response Body (for example, in PATCH, PUT, POST and Batched Requests)

Examples:
In this example the field "external_id" is to be updated, for this request, "fields" is a "StringKeyedDict" which can be imagined as a JSON object of the form `{"field1": "value1", "field2": "value2"}`

`PATCH /my_xplan_site/resourceful/enttiy/client-v3/1234?fields.external_id=abc123`

In this example, we will fetch the entity's external_id field:
In this case, fields is an array of the name's of the fields. (Of the form ["field1", "field2"])

`GET /my_xplan_site/resourceful/entity/client-v3/1234?fields.0=external_id`

(See Types for an understanding of how method parameters are converted to Query String Parameters

Request Body Parameters
-----------------------

For PATCH, PUT and POST requests, you can pass the parameters in the body in JSON format.

For example:
`PATCH /my_xplan_site/resourceful/entity/client-v3`
With Body:
`{ "fields": {"external_id": "abc123"}}`

Response Body:
Every API call responses includes headers and optional JSON-Formatted Body

HTTP Status Codes
=================

Successful requests
------------------
- 200 - Success. GET, PUT, PATCH will return a copy of the resource in the body
- 201 - Successful creation. a POST request to create an item will return the item in the body of the response
- 204 - Success with an empty body. This is returned by a successful DELETE request.

Failed Requests
---------------
- 400 - Bad request. The parameters specified by your client are invalid
- 401 - Unauthorized. The credentials provided were invalid for this resource. *Re-authenticate on this*
- 403 - Permission error. The request was valid, but the session does not have the required XPLAN Capabilities to access this resource or perform the requested operation.
- 404 - Not Found. This can mean that the resource is missing, moved, deleted. OR a typo in your request. OR the specific entry-point you requested is not available on this server. \*
- 500 - Unspecified Server Error - This is NEVER supposed to happen, if you receive HTTP 500, do not try and parse response, it could be invalid. Report this to IRESS at: XPLANDeveloperSupport@iress.com.au
- 502 - Upstream server error. XPLAN failed to communicate with a third-party server. (possible another IRESS service) on your behalf.

\* There is no way to track on what version of XPLAN an entry-point will be available, so if you are using an older XPLAN server it is possible the 404 does NOT mean the resource is non-existent, but the entry-point doesn't exist. For example, when/if user-v3 comes along, requests to it on older servers will ALWAYS HTTP 404.

For HTTP 500 the server may not return a JSON object like every other request will, thus you must be careful in processing any response returned.

For all non-500 errors, the server may return a JSON object containing details of the error.

Fields:
- user_message	Message to give to user about the error
- api_message	Message for developer to diagnose what actual caused the error

References
==========
- [Getting Started with XPLAN API](https://insights.iressconnect.com/docs/DOC-7376)
- [JSON](https://www.json.org/)
- [The JSON Data Interchange Syntax](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf)
