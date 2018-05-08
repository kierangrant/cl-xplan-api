Transactions
============

Transactions in XPLAN API are ambient transactions that any request can use them to do work as part of them.
You can use them to do a series of requests in a transacted "all or nothing" way.

In XPLAN API, a transaction's ID is used to associate any request to the transaction.
If any request in the transaction fails, the effect of preceding requests are rolled back.

Create a Transaction
---------------------

POST to: `my_xplan_site/resourceful/session/transaction`
Response is a Transaction ID String

Associate a Request with a Transaction
--------------------------------------

You must include a path parameter `_transaction` with the ID provided.
If the transaction has expired or is invalid you will get a HTTP 404 error.

If using a Bulk Request, it is included as a parameter in the "body" of a request not in the request URL.

**NOTE**: When appending to Request URL for non batch requests make sure you DO NOT URL Encode it...
That is, do not convert "=" to "%3D" otherwise the XPlan Server will give an error about invalid Transaction ID.

Commit a Transaction
--------------------

Use Custom HTTP Method COMMIT to commit a transaction

You can use POST with path parameter _method set to COMMIT (not with batched)

`POST my_xplan_site/resourceful/transaction/ID?_method=commit`
OR
`COMMIT my_xplan_site/resourceful/transaction/ID`

Should return 204 on success with no Body Content.

Using a Batch Request set the method to "COMMIT"

Rollback a Transaction
----------------------

Use the Delete method on the Transaction

Timeouts
--------

The Transaction will expire in 5 minutes after the resource is generated.
Thus if it is not committed within 5 minutes, the effects will be rolled back.

**Please do not rely on this, do rollback yourself to free server resources**

References
==========

 - [XPLAN API Transaction](https://insights.iressconnect.com/docs/DOC-7377)
