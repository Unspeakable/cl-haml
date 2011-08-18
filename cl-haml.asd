(in-package :cl-user)
(defpackage :cl-haml-asd (:use :cl :asdf))
(in-package :cl-haml-asd)

(defsystem cl-haml
  :serial t
  :author "Hiroyuki Tokunaga <inuzini.jiro@gmail.com>"
  :license "MIT License"
  :depends-on (:split-sequence :cl-ppcre :metatilities :cl-who)
  :components ((:file "package")
               (:file "var")
               (:file "reader")
               (:file "execute")))

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
