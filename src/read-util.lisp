(in-package :cl-haml)

(defun .id (&rest args)
  (format nil "~{~A~^_~}" args))

(defun .class (&rest args)
  (format nil "~{~A~^ ~}" args))

(defun flatten (list)
  "tree -> flat list."
  (cond ((atom list) list)
        ((listp (car list))
           (append (flatten (car list))
                   (flatten (cdr list))))
        (t
           (append (list (car list))
                   (flatten (cdr list))))))


(defun |-_a-zA-Z0-9p| (char)
  "[-_a-zA-Z0-9]"
  (and char
       (or (char= #\- char)
           (char= #\_ char)
           (char<= #\0 char #\9)
           (char<= #\a char #\z)
           (char<= #\A char #\Z))))


(defun get-insert-point (type sexp)
  (case type
    (+haml+ (if (eql 'cl-who:htm (car sexp))
                (car (last sexp))
                (last sexp)))
    (+lisp+ (last sexp))
    (+filter+ (when (listp sexp)
                (last sexp)))))

(defun concat-continue-lines (in line)
  ""
  (when line
    (loop :for len-1 := (1- (length line))
          :while (char= (char line len-1) #\\)
          :do (setf line
                    (concatenate 'string
                                 (subseq line 0 len-1)
                                 " "
                                 (string-trim *whitespace-chars*
                                              (read-line in))))))
  line)
