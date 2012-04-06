(cl:in-package :cl-user)

(defpackage :cl-haml
  (:use :cl)
  (:import-from :cl-who
                :str
                :esc
                :htm
                :with-html-output-to-string)
  (:shadow :defconstant)
  (:export #:*function-package*
           #:*html-mode*
           #:*escape-html*
           #:*doctypes*
           #:*output-indent-p*

           #:execute-haml
           #:register-haml

           #:.id
           #:.class

           #:clear-haml-all
           #:clear-haml
           #:clear-haml-all-files))
