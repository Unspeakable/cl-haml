(defpackage :cl-haml-test
  (:use :cl :cl-haml :cl-test-more))

(in-package :cl-haml-test)

(deftest execute-haml
    ""
  (let ((project-root (asdf:system-source-directory :cl-haml)))
    (labels ((file->string (pathname)
               (with-open-file (in pathname :direction :input :external-format :utf-8)
                 (with-output-to-string (out)
                   (loop for c = (read-char in nil nil) while c
                         do (write-char c out))))))
      (with-input-from-string (in-1 (file->string (merge-pathnames
                                                   "test/test-html/test-01.html"
                                                   project-root)))
        (with-input-from-string (in-2 (execute-haml (merge-pathnames
                                                     "test/test-haml/test-01.haml"
                                                     project-root)))
          (loop for line-1 = (read-line in-1 nil nil)
                for line-2 = (read-line in-2 nil nil)
                while (and line-1 line-2)
                do (is line-1 line-2 :test #'equal)))))))

(run-test-all)

(finalize)