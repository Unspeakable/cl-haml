(in-package :cl-user)
(defpackage :cl-haml-asd (:use :cl :asdf))
(in-package :cl-haml-asd)

(defsystem cl-haml
  :serial t
  :version "0.1.0"
  :author "Hiroyuki Tokunaga <inuzini.jiro@gmail.com>"
  :license "MIT License"
  :depends-on (:cl-ppcre :metatilities :cl-who :anaphora)
  :components ((:file "src/package")
               (:file "src/var")
               (:file "src/reader")
               (:file "src/escape")
               (:file "src/writer")
               (:file "src/execute")
               (:file "src/helper")))

(defmethod perform ((o test-op) (c (eql (find-system :cl-haml))))
  (operate 'load-op :cl-haml-test)
  (operate 'test-op :cl-haml-test))

(defsystem cl-haml-test
  :components ((:module "test"
                :serial t
                :components ((:file "reader-test"))))
  :depends-on (:cl-haml :cl-test-more))

(defmethod perform ((o test-op) (c (eql (find-system :cl-haml-test))))
  (operate 'load-op :cl-haml-test))
