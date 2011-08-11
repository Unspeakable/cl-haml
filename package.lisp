(cl:in-package :cl-user)

(defpackage :cl-haml
  (:use :cl :jiro)
  (:import-from :metatilities
                :push-end)
  (:export #:haml-file
           #:haml-str
           #:haml
           #:*haml-file-root*
           #:define-haml-fn
           #:execute-haml))

(in-package :cl-haml)
