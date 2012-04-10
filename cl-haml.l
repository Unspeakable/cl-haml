
(provide "cl-haml")

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify")
  (require "xl-who")
  (require "ansi-loop")
  (load-library "src/package.lisp"))

(in-package :cl-haml)

(ansify::install)

(load-library "src/specials.lisp")
(load-library "src/read-util.lisp")
(load-library "src/read-element.lisp")
(load-library "src/read-insert.lisp")
(load-library "src/read-lisp-block.lisp")
(load-library "src/read-filter.lisp")
(load-library "src/reader.lisp")
(load-library "src/cl-haml.lisp")
(defun pathnamep (x)
  (declare (ignore x))
  nil)
(defun file-write-date (x)
  (file-write-time x))
