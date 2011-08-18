(in-package :cl-haml)

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
