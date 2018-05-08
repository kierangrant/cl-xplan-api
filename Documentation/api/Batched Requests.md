Batched Requests
================

Batching allows you to execute several API operations in a single HTTP request.
XPLAN API accepts Batched HTTP Requests (batched requests).

All batch requests are made by POSTing to:
`my_xplan_site/resourceful`

Batches are sent as a JSON Object.

| Field | Value |
| --- | --- |
| batch | Array of Logical HTTP requests |
| include_subtimings | Boolean (Optional). When explicitly set the value to true, each logic HTTP response will include a time field to show the time spent for processing the corresponding request |

Logical HTTP Request:

| Field | Type | Description |
| --- | --- | --- |
| method | String | One of "GET, "PUT", "POST", "PATCH", etc... |
| url | String | The segment the request is for. Begins with "/resourceful" |
| name | String | Optional. A unique name for the request. If the name is not supplied, a sequential number (starting from 0) is used as the name for the request. The name is case sensitive and can be used in JSONPath for querying data. The corresponding response uses the same name as the request. \* |
| body | JSON Object | Optional. The request body. Only supports JSON |
| omit_results_on_success	| Boolean | Optional. For a stand-alone request, the response will be omitted only when this value is explicitly specified to true. For a dependent request, the response will be omitted unless this value is explicitly specified to false |
| accept | String | Optional. Specify a certain media type that is acceptable in the response |

\* - cl-xplan-api currently makes this mandatory so we can verify we got all we asked for. Also, this is used to attach the response and response code to the right object in the Bulk Requests object.

Example:
A bulk request to `GET /resourceful/entity/client-v3/30597` with fields "first_name", "last_name" and "client_adviser".
Plus a bulk request for the Client's adviser's first and last name.
```json
{
  "batch": [
    {
      "name": "client",
      "body": {
        "fields": [
          "first_name",
          "last_name",
          "client_adviser"
        ]
      },
      "method": "GET",
      "url": "/resourceful/entity/client-v3/30597",
      "omit_results_on_success": false
    },
    {
      "name": "adviser",
      "body": {
        "fields": [
          "first_name",
          "last_name"
        ]
      },
      "method": "GET",
      "url": "/resourceful/entity/user-v2/{result=client:$.client_adviser}",
      "omit_results_on_success": false
    }
  ],
  "include_subtimings": false
}
```

Batched Responses
=================

Response is an array of JSON objects.

| Field | Type | Description |
| --- | --- | --- |
| code | Integer | HTTP response code. eg 200 |
| msg | String | HTTP response message for code. eg "OK" |
| name | String | Name of the corresponding request |
| body | String | String encoded JSON object. eg "{\"id\": 123, \"entity_name\": "Foo\"}" \* |
| headers | JSON Object | A collection of key/value pairs of HTTP Header information. eg "Content-Type": "application/json" |
| time | Number | Optional. Time taken to process request. Only available if include_subtimings is explicitly set to true |

\* - You must decode the string into a JSON object to access it's content

Example response to Example Request from before:
```json
[
  {
    "msg": "OK",
    "body": "{\"client_adviser\":30590,\"first_name\":\"Test\",\"last_name\":\"KTestclient\",\"id\":30597,\"entity_name\":\"KTestclient, Test\"}",
    "code": 200,
    "name": "client",
    "headers": {
      "Content-Type": "application/json"
    }
  },
  {
    "msg": "OK",
    "body": "{\"fields\":{\"first_name\":\"Kieran\",\"last_name\":\"Grant\"},\"id\":30590,\"entity_name\":\"Grant, Kieran\"}",
    "code": 200,
    "name": "adviser",
    "headers": {
      "Content-Type": "application/json"
    }
  }
]
```

JSON Path
=========

JSON Path allows the use of resources from one (or more) requests in subsequent requests.
See References

Errors
======

When a request has an error, the overall HTTP status will still be 200 OK, as other requests may have succeeded. You must check for this.
If an error occurs, the body will be the same JSON object as returned by a normal error, encoded into a string as per normal.

Transactions
============

See [Transactions](Transactions.md)

The requests inside a bulk request occur within an implicit transaction.
You can specify a transaction manually, you can also create a transaction inside a bulk request and reference it using JSONPath.

Limits
======

The limit on number of requests that can be in one batch is 100.
The body of a logical HTTP request only supports JSON.

References
==========

- [XPLAN API Batched Requests](https://insights.iressconnect.com/docs/DOC-7378)
- [JSONPath - XPath for JSON](http://goessner.net/articles/JsonPath/)
