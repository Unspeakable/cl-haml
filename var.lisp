(in-package :cl-haml)

(defparameter *doctype-table* (make-hash-table :test 'equal))
(defun set-doctype (key doctype &optional default)
  (setf (gethash key *doctype-table*) (cons doctype default)))
(set-doctype "XML" "<?xml version='1.0' encoding='~A'>" "utf-8")
(set-doctype "5" "<!DOCTYPE html>")
(set-doctype "1.1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
(set-doctype nil "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
(set-doctype "Strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
(set-doctype "Basic" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Basic 1.1//EN\" \"http://www.w3.org/TR/xhtml-basic/xhtml-basic11.dtd\">")
(set-doctype "Mobile" "<!DOCTYPE html PUBLIC \"-//WAPFORUM//DTD XHTML Mobile 1.2//EN\" \"http://www.openmobilealliance.org/tech/DTD/xhtml-mobile12.dtd\">")
(set-doctype "RDFa" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML+RDFa 1.0//EN\" \"http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd\">")

(defvar *view-lambda-table* (make-hash-table :test 'equal))

(defvar *haml-file-root* nil)

(defvar *haml-output* nil)

(defvar *line-number* 0)

(defvar *in-filter* nil)

(defvar *offset-stack* nil)

(defvar *tag-stack* nil)

(defconstant +eof+ 'eof)

(defconstant +filter+
  (if (boundp '+filter+) +filter+ "(:[a-z]*)?"))
(defconstant +lisp-block+
  (if (boundp '+lisp-block+) +lisp-block+ "(- .*)?"))
(defconstant +tag+
  (if (boundp '+tag+) +tag+ "(%[a-zA-Z0-9]*)?"))
(defconstant +id+
  (if (boundp '+id+) +id+ "(#[-_a-zA-Z0-9]+)?"))
(defconstant +classes+
  (if (boundp '+classes+) +classes+ "(\\.[-\\._a-zA-Z0-9]+)?"))
(defconstant +attr+
  (if (boundp '+attr+) +attr+ "({[^}]*})?"))
(defconstant +opt+
  (if (boundp '+opt+) +opt+ "(!?=)?"))
(defconstant +body+
  (if (boundp '+body+) +body+ "(.*)?"))
