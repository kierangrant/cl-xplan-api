Types
=====

The Types supported include the JSON primitive types plus the following XPLAN specific types.

XPLAN Types are passed around as a JSON object with the following fields:

| Field | Meaning |
| --- | --- |
| \_type | A String indicating the XPLAN Type |
| \_val | The Value represented in some JSON or XPLAN type |

Example:
```json
{"_type": "TypeName", "_val": "Value"}
```

Binary
------

A blob of binary data.
This is used to contain data in formats that cannot be encoded raw into JSON, such as multimedia attachments.

| Field | Value |
| --- | --- |
| \_type | "Binary" |
| _val |  A base64 encoded string value |

Example:
```json
{"_type": "Binary", "_val": "VGVzdCBWYWx1ZQ=="}
```
Decoded Value: `Test Value`

Date
----

This includes dates with and without a time component.
Dates may contain timezone information depending on the resource.

| Field | Value |
| --- | --- |
| \_type | "Date" |
| \_val | ISO8601 date as a String |

Example:
```json
{"_type": "Date", "_val": "2018-03-01"}
```

Decoded Value: `1st of March 2018. No Time`

```json
{"_type": "Date", "_val": "2018-03-01T10:00:00Z"}
```

Decoded Value: `1st of March 2018, 10:00:00AM GMT`

Time
----

A time of day, with no attached date.

| Field | Value |
| --- | --- |
| \_type | "Time" |
| \_val |  ISO8601 time as a String |

Example:
```json
{"_type": "Time", "_val": "10:15Z"}
```
Decoded Value: `10:15 AM GMT`

Decimal
-------

A numeric decimal value. This type is used where precision is essential.
Client apps should consider using a decimal library to manipulate these values to avoid loss of precision.

*Please Note: This is not just an arbitrary precision numbers, but may also include scientific notation.*

| Field | Value |
| --- | --- |
| \_type | "BigDecimal" |
| \_val | String of the value |

| Example Strings | Description |
| --- | --- |
| "12345678901234567890" | Simple arbitrary precision value, can be prefixed with either "+" or "-" sign and can be post-fixed with an arbitrary length decimal value |
| "1.23456e2" | Scientific Notation form. Can be prefixed with either "+" or "-" sign |

Example:
```json
{"_type": "BigDecimal", "_val": "89875517873681764"}
```
Decoded Value: `89875517873681764`

Currency
--------

A currency amount, with a specific country code and decimal value.

| Field | Value |
| --- | --- |
| \_type | "Currency" |
| \_val | JSON Object |

Sub JSON object

| Field | Value |
| --- | --- |
| code | ISO4217 currency code string |
| value | XPLAN Decimal Value |

Example:
```json
{"_type": "Currency", "_val": {"code": "AUD", "value": {"_type": "BigDecimal", "_val": "100"}}}
```
Decoded Value: `AUD$100`

Flattening
==========

Form encoded data using "application/x-www-form-urlencoded" for GET and POST requests contain a flat dictionary of key/value pairs. To support nested data, you need to flatten the data.

To do that you construct an effective key name be using the parent key names separated by a ".".
If the structure is an array, you index starting at 0.

Example
```json
{ "foo": ["a", "b", "c"],
  "bar": { "xplan": "api"} }
```

Will become: `foo.0=a&foo.1=b&foo.2=c&bar.xplan=api`

You do not need to flatten if you are passing in a body as application/json.
Note: GET requests do not read body content, so you must flatten in this case.

Example XPLAN Types Flattened:
A Currency Object is one of the most complex types to flatten:
`some_request_url?some_field._type=Currency&some_field._val.code=AUD&some_field._val.value._type=BigDecimal&some_field._val.value._val=100`

You can thus see how nested JSON objects are flattened.

References
=========

- [Getting Started with XPLAN API](https://insights.iressconnect.com/docs/DOC-7376)
- [JSON](https://www.json.org/)
- [The JSON Data Interchange Syntax](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf)
