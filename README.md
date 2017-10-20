CL-XPLAN-API is a LISP library that makes it easier to interact with IRESS XPLAN API.

Whilst this software is released under the LLGPL, to actually connect to an IRESS XPLAN API Server requires you to have an API Key from them.

You need a use agreement with IRESS, though, nothing stops you using this library with a server that duplicates their API.

Note: As of 2017-10-20 you need to patch JSON with json-patch.diff, alternatively you can load json-patch.lisp, after loading CL-JSON manually.

Documentation for XPLAN API is available (after registration) at:
https://insights.iressconnect.com/docs/DOC-7376 - Getting Started with XPLAN API
https://insights.iressconnect.com/docs/DOC-7377 - XPLAN API Transaction
https://insights.iressconnect.com/docs/DOC-7378#jive_content_id_Specifying_Request_Dependencies - XPLAN API Batched Requests

Has been tested on SBCL and CLISP on GNU/Linux and Windows.
Note: On Windows, you need to set the path to a CA file or directory.
This setting is stored in the drakma-settings on the xplan-session.
EG:
(setf (getf (cl-xplan-api/core:drakma-settings *sess*) :ca-file) "/Path/to/cacert.perm")

Please note, new entry-points are added all the time in newer XPlan Versions.
There is no way to keep track of this other then continually reading release notes, even worse, according to IRESS, we cannot version test for an entry-point.
We must just accept a HTTP 404 as probably meaning it isn't implemented on that version.
