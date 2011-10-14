(in-package :cl-haml)

(defstruct view name function timestamp)

(defclass cl-haml-builder ()
  ((views :accessor views
          :initarg :views
          :initform nil
          :type list)
   (func-package :accessor func-package
                 :initarg :package
                 :initform :cl-user)
   (reload-p :accessor reload-p
             :initarg :reload-p
             :initform t)
   (load-root :accessor load-root
              :initarg :load-root
              :initform nil
              :type pathname)
   (prologue :accessor prologue
             :initarg :prologue
             :initform "<!DOCTYPE html>")
   (attribute-quote-char :accessor attribute-quote-char
                         :initarg :attribute-quote-char
                         :initform #\')
   (empty-tag-end :reader empty-tag-end
                  :initarg :empty-tag-end
                  :initform " />"
                  :type string)))

(defun haml-file-path (builder selector)
  (merge-pathnames (concatenate 'string selector ".haml")
                   (load-root builder)))

(defun render (builder view-selector params)
  (let* ((view-selector (string view-selector))
         (view (or (find view-selector (views builder)
                         :key #'view-name
                         :test #'equal)
                   (let ((tmp (make-view-function builder
                                                  view-selector)))
                     (when tmp
                       (push tmp (views builder)))
                     tmp)
                   (error "Haml file (~A) not found by ~A."
                          view-selector
                          (load-root builder))))
        (timestamp (file-write-date (haml-file-path builder view-selector))))
    (when (and (reload-p builder)
               (< (view-timestamp view) timestamp))
      (setf view (make-view-function builder view-selector))
      (pushnew view (views builder) :key #'view-name :test #'equal))
    (funcall (view-function view) params)))

;(defun render-partial (view-selector))
(defun make-view-function (builder selector)
  (let ((haml-file (haml-file-path builder selector))
        (*package* (find-package (func-package builder))))
    (ignore-errors
      (make-view
         :name (string selector)
         :timestamp (file-write-date haml-file)
         :function (let ((cl-who::*attribute-quote-char*
                            (attribute-quote-char builder)))
                     (make-haml-fn haml-file))))))

(defun make-haml-fn (file)
  (multiple-value-bind (haml->cl-who doctype)
      (haml-file file)
    (let ((cl-who::*downcase-tokens-p* nil))
      (eval
        `(lambda (params)
           (declare (ignorable params))
           (let ,(mapcar (lambda (sym)
                           `(,sym (getf params ,(@sym->keyword sym))))
                         (@symbols haml->cl-who))
             (cl-who:with-html-output-to-string (out nil
                                                     :prologue ,doctype
                                                     :indent t)
               ,haml->cl-who)))))))
