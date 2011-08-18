(cl:in-package :cl-user)

(defpackage :cl-haml
  (:use :cl :anaphora)
  (:import-from :metatilities
                :push-end
                :flatten)
  (:export #:haml-file
           #:haml-str
           #:haml
           #:*haml-file-root*
           #:define-haml-fn
           #:execute-haml))

(in-package :cl-haml)
