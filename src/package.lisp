(cl:in-package :cl-user)

(defpackage :cl-haml
  (:use :cl)
  (:shadow :defconstant)
  (:export #:*function-package*
           #:*html-mode*
           #:*escape-html*
           #:*doctypes*

           #:execute-haml
           #:register-haml

           #:clear-haml-all
           #:clear-haml
           #:clear-haml-all-files))
