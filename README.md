CL-XPLAN-API is a LISP library that makes it easier to interact with IRESS XPLAN API.

This is the in development v2-next branch, see the master branch for the stable code.

Whilst this software is released under the LGPLv2.1+, to actually connect to an IRESS XPLAN API Server requires you to have an API Key from them.

You need a use agreement with IRESS, though, nothing stops you using this library with a server that duplicates their API.

cl-json is currently patched by cl-xplan-api to include some new functionality needed by this library.

Documentation for XPLAN API is available (after registration) at:
- [Getting Started with XPLAN API](https://insights.iressconnect.com/docs/DOC-7376)
- [XPLAN API Transaction](https://insights.iressconnect.com/docs/DOC-7377)
- [XPLAN API Batched Requests](https://insights.iressconnect.com/docs/DOC-7378)

Has been tested on SBCL and CLISP on GNU/Linux and Windows.
Note: On Windows, you need to set the path to a CA file or directory.
This setting is stored in the drakma-settings on the xplan-session.
EG:
```lisp
(setf
 (getf (cl-xplan-api/core:drakma-settings *sess*) :ca-file)
 "/Path/to/cacert.perm")
```
