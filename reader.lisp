(in-package :cl-haml)

(defun blank-string->nil (str)
  (unless (zerop (length (trim str)))
    str))

;;; ========================================
;;;
(defun get-tag (tag id classes)
  (when (or tag id classes)
    (if (and tag (/= 1 (length tag)))
        (str->keyword (subseq tag 1))
        :div)))

;;; ========================================
;;;
(defun get-attr (attr)
  (when attr
    (read-concat "(" (subseq attr 1 (1- (length attr))) ")")))

;;; ========================================
;;;
(defun get-id (id)
  (when id
    (subseq id 1)))

;;; ========================================
;;;
(defun get-classes (classes attr)
  (when classes
    (trim
      (concat (split-sequence:split-sequence
                 #\.
                 (subseq classes 1))
              " "
              (getf attr :class)))))

;;; ========================================
;;;
(defun get-body (body)
  (let ((body (blank-string->nil body)))
    (if (and body (not (empty-p body)))
        (let ((head-ch (char body 0)))
          (if (or (char= #\\ head-ch) (char= #\Space head-ch))
              (subseq body 1)
              body))
        body)))

;;; ========================================
;;;
(defun read-concat (&rest args)
  (read-from-string (apply #'concat args)))

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
      ("^(#<+filter+>)?(#<+lisp-block+>)?(#<+tag+>)?(#<+id+>)?(#<+classes+>)?(#<+attr+>)?(#<+opt+>)?(#<+body+>)?$" line)
    (let ((tag  (get-tag tag id classes))
          (attr (get-attr attr))
          (body (get-body body)))
      (when id
        (setf (getf attr :id) (get-id id)))
      (when classes
        (getf attr :class) (get-classes classes attr))
      (cond (filter
              `(:filter ,(str->keyword (subseq filter 1))))
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
        (if tag `(,tag ,@attr ,body) body))
      (:filter
        (setf *in-filter* t)
        (make-filter-result tag))
      (:lisp
        (read-concat "(" tag ")")
        #+nil(let ((lisp (read-concat "(" tag ")"))
              (htm (list 'cl-who:htm)))
          (metatilities:push-end htm lisp)
          (values lisp htm))))))

;;; ========================================
;;;
(defun make-filter-result (filter)
  (case filter
    (:javascript (list :script :type "text/javascript"))
    (:css (list :style :type "text/css"))
    #+nil(:lisp "(progn ")))

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
     (indent-level blank)))

(defun tag-p (node)
  (let ((node-car (car node)))
    (or (keywordp node-car)
        (member node-car
                '(cl-who:str cl-who:htm cl-who:esc cl-who:fmt)))))

;;; ========================================
;;;
(defun set-tag (tag &optional opt)
  (awhen (car *tag-stack*)
    (unless (or opt (not (tag-p tag)) (tag-p it))
      (push (length *tag-stack*)
            *offset-stack*)
      (set-tag (list 'cl-who:htm) t)))
  (swhen (car *tag-stack*)
    (metatilities:push-end tag it))
  (push tag *tag-stack*))


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
  (loop :for line := (read-line in nil +eof+)
        :do (incf *line-number*)
        :until (eq +eof+ line)
        :unless (or (empty-p (r-trim line))
                    (start= "!!!" line)
                    (start= "-#" line))
          :do (ppcre:register-groups-bind (blank rest)
                  ("^( *)([^ ].*)$" line)
                (let ((level (indent-diff blank)))
                  ;; level > 0 then down indent.
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
                          (concat
                             "\\"
                             (subseq line
                                     (* (- (length *tag-stack*)
                                           (length *offset-stack*))
                                        2)))))
                      (set-tag (make-result rest))))))
  (car (last *tag-stack*)))


(defun haml->sexp (stream)
  (let* ((*tag-stack* nil)
         (*in-filter* nil)
         (*offset-stack* nil)
         (*line-number* 0))
    (parse stream)))


(defun haml-file (path &key (external-format :utf-8))
  (with-open-file (in path
                      :direction :input
                      :external-format external-format)
    (haml->sexp in)))


(defun haml-str (str)
  (with-input-from-string (in str)
    (haml->sexp in)))


(defmacro haml (path &key (external-format :utf-8))
  (let ((out (gensym)))
    `(with-output-to-string (,out)
       (cl-who:with-html-output (,out *haml-output*
                                      :prologue "<!DOCTYPE html>"
                                      :indent t)
         ,(haml-file path :external-format external-format)))))
