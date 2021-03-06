Authentication
==============

Regular Authentication
---------------------

To authenticate with a username / password combination you also need an XPLAN API Key.
Talk to your IRESS Representative about the requirements to get one of these.

Authentication is done using HTTP Basic Authentication, do NOT pass username / password until you are challenged by the server, be ready to re-authenticate at any time.

You will receive a cookie, pass this back to the server instead of the HTTP Basic Authentication request until you receive a server error indicating you need to make another authentication request.

You must also pass at all times your XPLAN API Key using the "X-Xplan-App-id" Header

OAuth2
------

You need to register your app with IRESS before getting started.
A registered app is assigned a unique App ID and Secret which will be used in the OAuth flow.
The Secret should not be shared.

**STEP 1** - To make an authorisation request, redirect users to:
`my_xplan_site/oauth2/auth`

The following values are needed as path parameters:

| Field | Meaning |
| --- | --- |
| client_id | App ID issued to you by IRESS |
|response_type | Use value "code" |
| redirect_uri | URL to redirect back to |

If authentication is successful, user will be redirected back to:
`redirect_uri?code=CODEVALUE`

This will be an authorisation code, use this to get an access token.

**STEP 2** - To get an access token, make the request to (using POST): my_xplan_site/oauth2/token

Values:

| Field | Meaning |
| --- | --- |
| client_id | App ID issued by IRESS |
| client_secret | Issued when you created your app |
| redirect_uri | URL to redirect back to |
| grant_type | Use value "authorization_code" |
| code | A temporary authorisation code received in step 1. |

Response will be in JSON, successful response will include these fields in the returned object

| Field | Meaning |
| --- | --- |
| access_token | Your Access Token |
| token_type | "Bearer" - Token Type |
| expires_in | Time To Live of access token |
| refresh_token | Token to use to re-generate an access token. |

Only the access_token can be used to fetch resources, refresh_token can only be used to issue a new pair of access_token and refresh_token.

**STEP 3** - Access Resource
Add a HTTP header called "Authorization" of the form
`Authorization: bearer my_access_token`

**STEP 4** - Refresh Token
When access token is expired or becomes invalid, use refresh token to get a new access token.
You can also use the refresh token to get multiple access tokens.

Post to: `my_xplan_site/oauth2/token`

Parameters:

| Field | Meaning |
| --- | --- |
| client_id | App ID issued by IRESS |
| client_secret | Issued when you created your app |
| redirect_uri | URL to redirect back to |
| grant_type | Use value "refresh_token" |
| refresh_token | The refresh token |

Successful response is JSON object with following fields

| Field | Meaning |
| --- | --- |
| access_token | New Access Token |
| token_type | "bearer" |
| expires_in | Time To Live |
| refresh_token | New Refresh Token |

Testing Authentication
----------------------

For testing, instead of passing an API key and User/Pass combo, you can pass the XPLANID Cookie from a regular session.
This is to only be used for development and testing of code.
**NEVER TO BE USED IN PRODUCTION**
(This does not need an API Key for testing purposes)

References
==========

- [Getting Started with XPLAN API](https://insights.iressconnect.com/docs/DOC-7376)
- [XPLAN API OAuth 2.0](https://insights.iressconnect.com/docs/DOC-8606)
- [OAuth 2](https://oauth.net/2/)
- [RFC 6749 - The OAuth 2.0 Authorization Framework](https://tools.ietf.org/html/rfc6749)
