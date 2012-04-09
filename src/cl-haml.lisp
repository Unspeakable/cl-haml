(in-package :cl-haml)

(defun read-doctype (in)
  (format nil "~{~A~^~%~}"
          (loop for c = #1=(peek-char nil in nil +eof+)
                while (or (eql c #\!) (and (eql c #\-)))
                if (eql c #\!)
                  collect (cdr (assoc (read-line in)
                                       (cdr (assoc *html-mode* *doctypes*))
                                       :test #'string-equal)) :into result
                
                else
                  do (read-char in)
                      (if (eql #1# #\#)
                          (read-line in)
                          (progn
                            (unread-char #\- in)
                            (loop-finish)))
                finally (return result))))

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
   (delete nil
           (mapcar (lambda (x)
                     (and (symbolp x)
                          (char= #\$ (char (string x) 0))
                          x))
                   (flatten sexp)))))

(defun make-haml-fn (stream)
  (multiple-value-bind (doctype body)
      (read-haml stream)
    (let ((args (haml-fn-args body))
          (env  (intern (string :env) *function-package*)))
      (compile nil
               `(lambda (,env)
                  (declare (ignorable ,env))
                  (let (,@(mapcar (lambda (arg)
                                    `(,arg (getf ,env
                                                 (intern (subseq (string ',arg) 1)
                                                         :keyword))))
                                  args))
                    (with-html-output-to-string (out
                                                 nil
                                                 :indent ,*output-indent-p*
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

(defstruct haml-function
  path time function)

(defun clear-haml-all ()
  "Remove all registered CL-HAML code."
  (with-lock
    (clrhash *functions*)))

(defun clear-haml (name)
  "Remove named CL-HAML code."
  (with-lock
    (remhash name *functions*)))

(defun get-haml-function (name)
  "Returns the named function implementing a registered CL-HAML code.
Rebuilds it when text template was a file which has been modified."
  (with-lock
    (let* ((haml-function (gethash name *functions*))
           (path (when haml-function (haml-function-path haml-function))))
      (cond ((null haml-function)
               (return-from get-haml-function))
            ((and path
                  (or (pathnamep path)
                      (not (find #\LineFeed path)))
                  (> (file-write-date path)
                     (haml-function-time haml-function)))
               ;; Update when file is newer
               (register-haml name name haml-function)))
      (haml-function-function haml-function))))

(defun register-haml (name code &optional obj)
  "Register given $var{CODE} as $var{NAME}."
  (let (function
        (string-haml-p (and (not (pathnamep code))
                            (find #\LineFeed code))))
    (if string-haml-p
        (with-input-from-string (stream code)
          (setf function (make-haml-fn stream)))
        (with-open-file (stream code
                                :direction :input
                                :external-format :utf-8)
          (setf function (make-haml-fn stream))))
    (with-lock
      (setf (gethash name *functions*)
            (if obj
                (setf (haml-function-time obj)
                        (file-write-date code)
                      (haml-function-function obj)
                        function)
                (make-haml-function :path code
                                    :time (if string-haml-p
                                              (get-universal-time)
                                              (file-write-date code))
                                    :function function))))))

(defun execute-haml (name &key env)
  "Execute named CL-HAML code. Returns a string.
   Keyword parameter $var{ENV} to pass objects to the code.
   $var{ENV} must be a plist."
  (funcall (or (get-haml-function name)
               (haml-function-function (register-haml name name)))
           env))
