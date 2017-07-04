(in-package :json)

(defconstant +JSON-FALSE+ '+JSON-FALSE+ "Special Constant to force the output of 'false' in an encoded JSON structure. Some applications are known to test for a value of 'false', meaning that a value of 'null' is not enough, this value can be used for those special occasions.")

(defparameter +json-lisp-symbol-tokens+
  '(("true" . t)
    ("null" . nil)
    ("false" . nil)
    ("false" . +JSON-FALSE+)))

(export +JSON-FALSE+)
