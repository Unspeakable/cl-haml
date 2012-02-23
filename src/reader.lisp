(in-package :cl-haml)

(defun haml-reader-dispatch (parent-type
                             stream
                             &optional (eof-error-p nil)
                                       (eof-value +eof+))
  (ecase parent-type
    (+haml-multiple-comment+
       (read-line stream eof-error-p eof-value)
       `(,parent-type))
    (+filter+
       `(,parent-type ,(read-line stream eof-error-p eof-value)))
    ((+haml+ +lisp+)
       (case (peek-char nil stream eof-error-p eof-value)
         ((#\% #\# #\.)
            (let ((result (read-haml-element-line stream
                                                  eof-error-p
                                                  eof-value)))
              (when (eq parent-type +lisp+)
                (setf (cadr result) `(cl-who:htm ,(cadr result))))
              result))
         ((#\! #\& #\=)
            (let ((result (read-haml-insert-line stream
                                                 eof-error-p
                                                 eof-value)))
              (when (eq parent-type +lisp+)
                (setf (cadr result) `(cl-who:htm ,(cadr result))))
              result))
         ((#\-)
            (read-char stream)
            (case (peek-char nil stream eof-error-p eof-value)
              (#\#	    ; Haml comment
                 (read-char stream)
                 (read-haml-comment stream eof-value))
              (#\Space      ; Haml Lisp Block
                 (read-haml-lisp-block stream))))
         ((#\:)
            `(+filter+ ,(read-filter-line stream)))
         (t
            (let ((line (read-line stream eof-error-p eof-value)))
              (if (and (not (eql eof-value line))
                       (char= #\\ (char line 0)))
                  (setf line (subseq line 1)))
              `(:text ,(if (eq parent-type +lisp+) `(cl-who:htm ,line) line))))))))


(defun eol-or-eof-p (char eof-value)
  (or (eq eof-value char) (char= #\Newline char)))

(defun read-haml-line (parent-type stream &optional (eof-error-p nil)
                                                    (eof-value +eof+))
  (list
   ;; Parsed content. (type &optional content)
   (haml-reader-dispatch parent-type
                         stream
                         eof-error-p
                         eof-value)
   ;; Next line indent.
   (loop :with blank-count := 0
         :for c := (read-char stream eof-error-p eof-value)
         :if (or (eq c eof-value)
                 (let ((code (char-code c)))
                   (or (<= 33 code 127)
                       (<= 128 code))))
             :return (let ((code (if (eq c eof-value)
                                     -1
                                     (char-code c))))
                       (when (<= 33 code)
                         (unread-char c stream))
                       blank-count)
         :else if (char= c #\Space)
             :do (incf blank-count)
         :else if (char= c #\Newline)
            :do (setf blank-count 0))
   ;; EOF?
   (eq eof-value
       (peek-char nil stream eof-error-p eof-value))))

(defun read-haml-body (stream indent parent-type)
  ""
  (loop :for ((type content) next-indent eof?)
          := (read-haml-line parent-type stream)
        :for set-point := (get-insert-point type content)
        :collect content :into contents
        :if (and (not (eql +haml-multiple-comment+ parent-type))
                 (not (eql +filter+ parent-type))
                 (< indent next-indent))
          :do (multiple-value-bind (tmp-indent tmp-contents tmp-eof?)
                  (read-haml-body stream next-indent type)
                (setf next-indent tmp-indent)
                (setf eof? tmp-eof?)
                (setf set-point
                      (nconc set-point tmp-contents)))
        :until (or eof? (< next-indent indent))
        :finally (return (values next-indent contents eof?))))
