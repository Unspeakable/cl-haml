(in-package #-xyzzy :cl-user
            #+xyzzy :user)

(defpackage :cl-haml
  (:use #-xyzzy :cl
        #+xyzzy :lisp)
  (:import-from :who
                :str
                :esc
                :htm
                :with-html-output-to-string)
  #+xyzzy
  (:shadowing-import-from :ansi-loop
                          :loop
                          :loop-finish)
  #+sbcl
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
