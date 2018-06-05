cl-xplan-api is build-able using the [asdf system](https://common-lisp.net/project/asdf/).

The main target audience for this library is server-side software, as such, this code is in LISP, because on a server you can use whatever language you like.

Development is done with EMACS using SLIME, using SBCL.

These are the direct dependencies for cl-xplan-api, they will depend on others in turn. You can use quicklisp to fetch the required systems.

- Drakma - HTTP Client.
- cl-json - JSON for Common LISP. Patched by cl-xplan-api to allow explicit false values
- json-to-clos - Simple JSON to CLOS mapper macro, depends on cl-json.
- Babel - Convert To and From Unicode plus multi-language support
- Decimals - Simple Decimal to String conversion. (LISP already has arbitrary precision numbers)
- cl-base64 - Convert To and From Base64
- rw-ut - Read and Write to ISO 8601 Date / Times to LISP Universal Time. (Unfortunately, LISP dates cannot go before 1900-01-1 00:00 GMT, which XPLAN uses... :/) Oh well.
- split-sequence - For breaking sequences up into smaller parts. Used for reading in Scientific notation Big Decimals That XPLAN Sometimes throws. Also used to get domain for testing session... because I'm lazy and didn't want to write that myself...

Once you have either loaded quicklisp, or downloaded all dependencies (recursively) somewhere and put links in the ~/common-lisp/ directory (or setup ASDF Source Registry) it should be as easy as:
```lisp
(require "asdf")
(asdf:make "cl-xplan-api")
;; Done!
```

If you are using Quicklisp, follow these instructions:
```shell
mkdir ~/common-lisp/
cd ~/common-lisp/
git clone https://gitlab.com/kierangrant/cl-xplan-api.git
git clone https://gitlab.com/kierangrant/json-to-clos.git
git clone https://github.com/nallen05/rw-ut.git # not available through Quicklisp
wget https://beta.quicklisp.org/quicklisp.org
```
```lisp
sbcl --load quicklisp.lisp
;Then install quicklisp with
(quicklisp-quickstart:install)
# or if Quicklisp is already installed
sbcl --load ~/quicklisp/setup.lisp
;; Now lets load everything
(ql:quickload "cl-xplan-api")
;; Now lets do a test
(defpackage :test (:use :cl :cl-xplan-api))
(in-package :test)
(defvar *session*
  (make-instance
   'xplan-session
   :base-url "YourXPLANSite"
   :username "User"
   :password "Password"
   :api-key "API-KEY"))
; Or for testing
(load "~/common-lisp/cl-xplan-api/src/core/testing")
(defvar *session*
  (make-instance
   'cl-xplan-api/core::xplan-session-test
   :base-url "YourXPLANSite"
   :xplanid "XPLANIDCOOKIE"))

(defvar *session/user* (xplan-call *session* "session/user" :get))	
; returns a Hash Table. (For multiple response entry-points, it returns a vector of hash tables)
(maphash (lambda (k v) (format t "~S: ~S~%" k v)) *session/user*)
```

**NOTE**: If you are on Windows, you will need to Download/Build OpenSSL for your Common LISP environment and have a cacerts.txt file or a CA Directory to setup Drakma to point towards.
For example:
```lisp
(setf (getf (cl-xplan-api/core:drakma-settings *session) :ca-file) "/path/to/cacert.crt")
```

Resources
---------
- [GNU Emacs](https://www.gnu.org/software/emacs/)
- [SLIME](https://common-lisp.net/project/slime/)
- [SBCL](http://www.sbcl.org/)
- [Quicklisp](https://www.quicklisp.org/beta/)
