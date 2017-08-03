;;; Only try and define +JSON-FALSE+ if it is not in CL-JSON already.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (not (find-symbol "+JSON-FALSE+" (find-package "CL-JSON")))
      (progn
	(in-package :json)
	(defconstant +JSON-FALSE+ '+JSON-FALSE+ "Special Constant to force the output of 'false' in an encoded JSON structure. Some applications are known to test for a value of 'false', meaning that a value of 'null' is not enough, this value can be used for those special occasions.")
	(defparameter +json-lisp-symbol-tokens+
	  '(("true" . t)
	    ("null" . nil)
	    ("false" . nil)
	    ("false" . +JSON-FALSE+)))
	(export +JSON-FALSE+))))
