(in-package :cl-haml)

(defun read-doctype (in)
  (format nil "~{~A~^~%~}"
          (loop :while (eql (peek-char nil in nil +eof+) #\!)
                :collect (cdr (assoc (read-line in)
                                     (cdr (assoc *html-mode* *doctypes*))
                                     :test #'string-equal)))))

(defun read-haml (stream)
  (let ((*package* (or (find-package *function-package*)
                       (error "Package not Found: ~S" *function-package*))))
    (values (read-doctype stream)                  ; Read DOCTYPE(^!!! ?.*$)
            (multiple-value-bind (_1 result _2)    ; Read Haml Body
                (read-haml-body stream 0 +haml+)
              (declare (ignore _1 _2))
              result))))

(defun haml-fn-args (sexp)
  (delete-duplicates
   (delete nil (mapcar (lambda (x)
                         (and (symbolp x)
                              (char= #\$ (char (string x) 0))
                              x))
                       (flatten sexp)))))

(defun make-haml-fn (stream)
  (multiple-value-bind (doctype body)
      (read-haml stream)
    (let ((args (haml-fn-args body)))
      (compile nil
               `(lambda (env)
                  (declare (ignorable env))
                  (let (,@(mapcar (lambda (arg)
                                    `(,arg (getf env
                                                 (intern (subseq (string ',arg) 1)
                                                         :keyword))))
                                  args))
                    (cl-who:with-html-output-to-string (out
                                                        nil
                                                        :indent 0
                                                        :prologue ,doctype)
                      ,@body)))))))

;;;
(defvar *locking-function* nil
  "Function to call to lock access to an internal hash table.
Must accept a function designater which must be called with the lock hold.")

(defmacro with-lock (&body body)
  "Locking all accesses to *functions*"
  `(cond (*locking-function*
           (funcall *locking-function*
                    (lambda ()
                      ,@body)))
         (t ,@body)))

(defvar *functions* (make-hash-table :test #'equal)
  "Table mapping names to haml-function instances.")

(defclass haml-function ()
  ((path
      :initarg :path
      :accessor haml-function-path)
   (time
      :initarg :time
      :accessor haml-function-time)
   (function
      :initarg :function
      :accessor haml-function-function)))

(defun make-haml-function (path time function)
  "Constructor for class EMB-FUNCTION."
  (make-instance 'haml-function
                 :path path
                 :time time
                 :function function))

(defun clear-haml-all ()
  "Remove all registered CL-HAML code."
  (with-lock
    (clrhash *functions*)))

(defun clear-haml (name)
  "Remove named CL-HAML code."
  (with-lock
    (remhash name *functions*)))

(defun clear-haml-all-files ()
  "Remove all registered file CL-HAML code(registered/executed by a pathname)."
  (with-lock
    (maphash (lambda (key value)
               (declare (ignore value))
               (when (typep key 'pathname)
                 (remhash key *functions*)))
             *functions*)))

(defun get-haml-function (name)
  "Returns the named function implementing a registered CL-HAML code.
Rebuilds it when text template was a file which has been modified."
  (with-lock
    (let* ((haml-function (gethash name *functions*))
           (path (when haml-function (haml-function-path haml-function))))
      (cond ((and (not (typep name 'pathname))
                  (null haml-function))
               (error "Function ~S not found." name))
            ((null haml-function)
               (return-from get-haml-function))
            ((and path
                  (> (file-write-date path)
                     (haml-function-time haml-function)))
               ;; Update when file is newer
               (with-open-file (stream name :direction :input :external-format :utf-8)
                 (setf (haml-function-time haml-function) (file-write-date path)
                       (haml-function-function haml-function) (make-haml-fn stream)))))
      (haml-function-function haml-function))))

(defgeneric register-haml (name code)
  (:documentation "Register given $var{CODE} as $var{NAME}."))

(defmethod register-haml (name (code pathname))
  (let (function)
    (with-open-file (stream code :direction :input :external-format :utf-8)
      (setf function (make-haml-fn stream)))
    (with-lock
      (setf (gethash name *functions*)
            (make-haml-function code
                                (file-write-date code)
                                function)))))

(defmethod register-haml (name (code string))
  (let (function)
    (with-input-from-string (stream code)
      (setf function (make-haml-fn stream)))
    (with-lock
      (setf (gethash name *functions*)
            (make-haml-function code
                                (get-universal-time)
                                function)))))

(defgeneric execute-haml (name &key env)
  (:documentation "Execute named CL-HAML code. Returns a string. Keyword parameter
$var{ENV} to pass objects to the code. $var{ENV} must be a plist."))

(defmethod execute-haml ((name string) &key env)
  (funcall (get-haml-function name)
           env))

(defmethod execute-haml ((name pathname) &key env)
  (funcall (or (get-haml-function name)
               (haml-function-function (register-haml name name)))
           env))
