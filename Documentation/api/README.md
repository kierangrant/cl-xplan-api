These files details the aspects of the XPLAN API and how to make calls using it.

XPLAN API is a HTTP Rest API.

1. [Protocol](Protocol.md) - Information about core protocol, where to make requests and the underlying protocol.
2. [Authentication](Authentication.md) - How to authenticate with XPLAN
3. [Types](Types.md) - XPLAN Types, how they are passed and flattened
4. [Batched Requests](Batched%20Requests.md) - Details how to do Batched Requests
5. [Transactions](Transactions.md) - How to do transactions

These files are an outline, please refer to the [IRESS Insights community](https://insights.iressconnect.com/community/developers/xplan-api/content) for an up-to-date reference.

Extra Notes
-----------

Some Extra notes will appear in the other files if possible, otherwise they will be included here:

Bookmarks
---------

Just like as in Transactions, the page_bookmark parameter when used in a GET request, must also NOT be URL-encoded... otherwise XPlan throws errors.

That is, do not convert "=" to "%3D".

This only affects non Batched Requests. Batched Requests have no URL encoding.
