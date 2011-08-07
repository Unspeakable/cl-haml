(asdf:defsystem cl-haml
  :serial t
  :author "Hiroyuki Tokunaga <inuzini.jiro@gmail.com>"
  :license "LLGPL"
  :depends-on (:split-sequence :cl-ppcre :jiro :metatilities :cl-who)
  :components ((:file "package")
               (:file "var")
	       (:file "reader")))
