(in-package :cl-haml)

(defun read-haml-lisp-block (stream)
  (let ((line (concat-continue-lines stream (read-line stream nil nil))))
    (with-input-from-string (in line)
      (list +lisp+
            `(,@(loop :for x := (read in nil nil) :while x :collect x))))))

(defun read-haml-comment (stream eof-value)
  (let ((line (read-line stream nil eof-value)))
    (list (if (or (eq line eof-value)
                  (zerop (length (string-trim *whitespace-chars* line))))
              +haml-multiple-comment+
              +haml-line-comment+))))
