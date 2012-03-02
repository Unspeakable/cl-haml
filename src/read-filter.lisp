(in-package :cl-haml)

(defvar *filter-convertion*
  `((":javascript" . ((:|script| :|type| "text/javascript") . #'identity))
    (":css" . ((:|style| :|type| "text/css") . #'identity))))

(defun read-filter-line (stream)
  (let ((line (read-line stream nil nil)))
    (copy-list (cadr (assoc (string-trim *whitespace-chars* (or line ""))
                            *filter-convertion*
                            :test #'string-equal)))))
