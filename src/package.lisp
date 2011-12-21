(cl:in-package :cl-user)

(defpackage :cl-haml
  (:use :cl :anaphora)
  (:import-from :metatilities
                :push-end
                :flatten)
  (:export #:haml-file
           #:haml
           #:*haml-file-root*
           #:define-haml-fn
           #:execute-haml

           #:render
           #:cl-haml-builder
           #:views
           #:reload-p
           #:load-root
           #:prologue
           #:attribute-quote-char
           #:empty-tag-end

           #:set-doctype
           #:stylesheet-link-tag
           #:javascript-include-tag
           #:link-to
           #:post-to))
