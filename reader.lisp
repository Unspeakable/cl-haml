(in-package :cl-haml)

(defvar *white-space-chars* '(#\Space #\Return #\LineFeed #\Tab #\Page))
(defvar *line-number* 0)
(defvar *in-filter* nil)
(defvar *offset-stack* nil)
(defvar *tag-stack* nil)

;;; ========================================
;;;
(defun |read-preserving| (&optional (stream *standard-input*)
                                    (eof-error-p t)
                                    eof-value)
  (flet ((skip-whitespace ()
           (loop :for c := (peek-char nil stream eof-error-p eof-value)
              :while (member c
                             *white-space-chars*
                             :test #'equal)
              :until (equal c eof-value)
              :do (read-char stream eof-error-p eof-value))))
    (skip-whitespace)
    (let* ((str? (equal (peek-char nil stream eof-error-p eof-value) #\"))
           (result
            (loop :for c := (read-char stream eof-error-p eof-value)
               :for i := 0 :then (1+ i)
               :until (equal c eof-value)
               :until (if str?
                          (and (not (zerop i)) (char= c #\"))
                          (member c *white-space-chars* :test #'equal))
               :finally (unless (or (equal c eof-value)
                                    (and str? (equal c #\")))
                          (unread-char c stream))
               :collect c)))
      (cond ((null result) eof-value)
            (str? (coerce (cdr result) 'string))
            ((char= #\: (car result))
             (intern (coerce (cdr result) 'string) :keyword))
            (t (intern (coerce result 'string)))))))

;;; ========================================
;;;
(defun blank-string->nil (str)
  (unless (zerop (length (string-trim *white-space-chars* str)))
    str))

;;; ========================================
;;;
(defun get-tag (tag id classes)
  (when (or tag (blank-string->nil id) (blank-string->nil classes))
    (if (and tag (/= 1 (length tag)))
        (intern (subseq tag 1) :keyword)
        :|div|)))

;;; ========================================
;;;
(defun get-attr (attr)
  (when (and attr (<= 2 (length attr)))
    (let (result
          (eof 'eof))
      (with-input-from-string (in (subseq attr 1 (1- (length attr))))
        (loop :for sym := (|read-preserving| in nil eof)
              :until (eq eof sym)
              :do (push sym result)
                  (let ((value (read in nil eof)))
                    (when (eq eof value)
                      (error "Unexpected End of File occurred while reading the Attribute."))
                    (push value result))))
      (nreverse result))))

;;; ========================================
;;;
(defun get-id (id)
  (when (and id (<= 1 (length id)))
    (subseq id 1)))

;;; ========================================
;;;
(defun get-classes (classes attr)
  (string-trim *white-space-chars*
    (concatenate
       'string
       (getf attr :|class|)
       (substitute #\Space #\. classes))))

;;; ========================================
;;;
(defun get-body (body)
  (let ((body (blank-string->nil body)))
    (when body
      (let ((head-ch (char body 0)))
        (if (or (char= #\\ head-ch) (char= #\Space head-ch))
            (subseq body 1)
            body)))))

;;; ========================================
;;;
(defun read-concat (&rest args)
  (read-from-string (apply #'concatenate 'string args)))

;;; ========================================
;;;
(defun gen-standard-result (tag attr body opt)
  (let ((body (if opt
                  (if (string= opt "!=")
                      (read-concat "(cl-who:str " body ")")
                      (read-concat "(cl-who:esc (princ-to-string " body "))"))
                  body)))
    `(:standard ,tag ,attr ,body)))

;;; ========================================
;;;
(defun split-line (line)
  (ppcre:register-groups-bind (filter lisp-block tag id classes attr opt body)
      ((concatenate 'string
                    "^" +filter+ +lisp-block+ +tag+ +id+ +classes+
                    +attr+ +opt+ +body+ "$") line)
    (let ((tag  (get-tag tag id classes))
          (attr (get-attr attr))
          (body (get-body body)))
      (when id
        (setf (getf attr :|id|) (get-id id)))
      (when classes
        (setf (getf attr :|class|) (get-classes classes attr)))
      (cond (filter
              `(:filter ,(intern (subseq filter 1) :keyword)))
            (lisp-block
              `(:lisp ,(subseq lisp-block 2)))
            (t
              (gen-standard-result tag attr body opt))))))

;;; ========================================
;;;
(defun make-result (rest)
  (destructuring-bind (switch tag &optional attr body)
      (split-line rest)
    (case switch
      (:standard
        (if tag
            `(,tag ,@attr ,body)
            body))
      (:filter
        (setf *in-filter* t)
        (make-filter-result tag))
      (:lisp
        (read-concat "(" tag ")")))))

;;; ========================================
;;;
(defun make-filter-result (filter)
  (case filter
    (:|javascript| (list :|script| :|type| "text/javascript"))
    (:|css| (list :|style| :|type| "text/css"))))

;;; ========================================
;;;
(defun indent-level (blank)
  (let ((blank-length (length blank)))
    (if (oddp blank-length)
        (error "Syntax error on line ~D:
  The line was indented ~D levels deeper than the line."
               *line-number*
               blank-length)
        (/ blank-length 2))))

;;; ========================================
;;;
(defun indent-diff (blank)
  (- (length *tag-stack*)
     (length *offset-stack*)
     (indent-level blank)
     1))

(defun tag-p (node)
  (unless (atom node)
    (let ((node-car (car node)))
      (or (keywordp node-car)
          (member node-car
                  '(cl-who:str cl-who:htm cl-who:esc cl-who:fmt))))))

;;; ========================================
;;;
(defun set-tag (tag &optional opt)
  (awhen (car *tag-stack*)
    (unless (or opt (not (tag-p tag)) (tag-p it))
      (push (length *tag-stack*)
            *offset-stack*)
      (set-tag (list 'cl-who:htm) t)))
  (if (atom tag)
      (set-text tag)
      (progn
        (swhen (car *tag-stack*)
          (metatilities:push-end tag it))
        (push tag *tag-stack*))))


;;; ========================================
;;;
(defun set-text (text)
  (metatilities:push-end text (car *tag-stack*)))


;;; ========================================
;;;
(defun start= (str1 str2)
  (let ((str1-len (length str1)))
    (and (<= str1-len (length str2))
         (string= str1 str2 :end2 str1-len))))


;;; ========================================
;;;
(defun parse (in)
  (let ((doctypes nil))
    (loop :for line := (read-line in nil +eof+)
          :do (incf *line-number*)
          :until (eq +eof+ line)
          :unless (or (zerop (length (string-right-trim *white-space-chars* line)))
                      (start= "!!!" line)
                      (start= "-#" line))
            :do (loop :while (char= (char line (1- (length line))) #\\)
                      :do (setf line (concatenate 'string
                                                  (subseq line 0 (1- (length line)))
                                                  (read-line in)))
                          (incf *line-number*))
                (ppcre:register-groups-bind (blank rest)
                    ("^( *)([^ ].*)$" line)
                  (let ((level (indent-diff blank)))
                    (when (plusp level)
                      (dotimes (i level)
                        (pop *tag-stack*)
                        (when (and *offset-stack*
                                   (= (car *offset-stack*)
                                      (length *tag-stack*)))
                          (pop *tag-stack*)
                          (pop *offset-stack*)))
                      (when *in-filter*
                        (setf *in-filter* nil)))

                    (if *in-filter*
                        (set-text
                          (make-result
                            (concatenate 'string
                               "\\"
                               (subseq line
                                       (* (- (length *tag-stack*)
                                             (length *offset-stack*)
                                             1)
                                          2)))))
                        (set-tag (make-result rest)))))
          :if (start= "!!!" line)
            :do (ppcre:register-groups-bind (type option)
                    ("^!!! ([\\.a-zA-Z0-9]+)? ?(-a-zA-Z0-9]+)?$" line)
                  (push (doctype type option) doctypes)))
    (values (car (last *tag-stack*))
            (format nil "~{~A~^~%~}" (nreverse doctypes)))))


(defun doctype (type option)
  (awhen (gethash type *doctype-table*)
    (format nil (car it) (or option (cdr it)))))


(defun haml->sexp (stream)
  (let* ((*tag-stack* (list (list 'cl-who:htm)))
         (*in-filter* nil)
         (*offset-stack* nil)
         (*line-number* 0))
    (parse stream)))


(defun haml-file (path &key (external-format :utf-8))
  (with-open-file (in path
                      :direction :input
                      :external-format external-format)
    (haml->sexp in)))

(defun haml (haml-path &optional out-path)
  (multiple-value-bind (sexp doctype)
      (haml-file haml-path)
    (eval
     (if out-path
         `(with-open-file (out ,out-path
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (cl-who:with-html-output (out out :prologue ,doctype :indent t)
              ,sexp))
         `(cl-who:with-html-output-to-string (out nil :prologue ,doctype :indent t)
            ,sexp)))))

(defun @symbols (lst)
  (remove-if-not
   (lambda (x)
     (and (symbolp x)
          (char= #\@ (char (symbol-name x) 0))))
   (remove-duplicates (flatten lst))))

(defun @sym->keyword (sym)
  (intern (subseq (string-upcase sym) 1) :keyword))
