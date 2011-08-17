(in-package :cl-haml)

(defstruct view-fn file-name timestamp lambda)

(defun execute-haml (file params &key (package *package*))
  (unless *haml-file-root*
    (error "CL-HAML:*HAML-FILE-ROOT* is nil."))

  (let ((it (gethash file *view-lambda-table*))
        (timestamp (file-write-date
                      (merge-pathnames file *haml-file-root*)))
        (*package* (find-package package)))
    (when (or (null it)
              (< (view-fn-timestamp it) timestamp))
      (setf (gethash file *view-lambda-table*)
            (make-view-fn :file-name file
                          :timestamp timestamp
                          :lambda (let ((cl-who:*downcase-tokens-p* nil))
                                    (make-haml-fn
                                     (merge-pathnames file *haml-file-root*))))))
    (funcall (view-fn-lambda (gethash file *view-lambda-table*)) params)))
