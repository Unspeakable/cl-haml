(in-package :cl-user)
(defpackage :cl-haml-asd
  (:use :cl :asdf))
(in-package :cl-haml-asd)

(defsystem cl-haml
  :serial t
  :description "Haml like XHTML generator."
  :version "1.0.2"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :author "Hiroyuki Tokunaga <inuzini.jiro@gmail.com>"
  :license "MIT License"
  :depends-on (:cl-who)
  :components ((:file "src/package")
               (:file "src/specials")
               (:file "src/read-util")
               (:file "src/read-element")
               (:file "src/read-insert")
               (:file "src/read-lisp-block")
               (:file "src/read-filter")
               (:file "src/reader")
               (:file "src/cl-who-patch")
               (:file "src/cl-haml")))

(defmethod perform ((o test-op)
                    (c (eql (find-system :cl-haml))))
  (operate 'load-op :cl-haml-test)
  (operate 'test-op :cl-haml-test))

(defsystem cl-haml-test
  :components ((:module "test"
                :serial t
                :components ((:file "reader-test")
                             (:file "cl-haml-test"))))
  :depends-on (:cl-haml :cl-test-more))

(defmethod perform ((o test-op)
                    (c (eql (find-system :cl-haml-test))))
  (operate 'load-op :cl-haml-test))
