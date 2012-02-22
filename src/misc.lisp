(in-package :cl-haml.intern)

(defun html-mode ()
  *html-mode*)

(defun (setf html-mode) (mode)
  "Sets the output mode to XHTML or HTML. Mode can be :XHTML for XHTML,
:HTML4 for HTML4 or :HTML5 for HTML5."
  (ecase mode
    (:html4
       (setf *html-mode* :html4
             *empty-tag-end* ">"))
    (:html5
       (setf *html-mode* :html5
             *empty-tag-end* ">"))
    (:xhtml
       (setf *html-mode* :xhtml
             *empty-tag-end* " />"))))


(defun count-indent (stream)
  (labels ((%count-indent (stream count)
             (let ((char (read-char stream nil nil)))
               (cond ((null char) 0)
                     ((char= #\Space char)
                        (%count-indent stream (1+ count)))
                     (t count)))))
    (%count-indent stream 0)))

(defun count-indent (line)
  (or (position #\Space line :test-not #'eql) 0))

(5am:test count-indent
  (5am:is (= 0 (count-indent nil)))
  (5am:is (= 0 (count-indent "")))
  (5am:is (= 2 (count-indent "  %hoge")))
  (5am:is (= 5 (count-indent "     %hoge"))))

(defun add-id (id attr-list)
  (let ((attr-list (copy-list attr-list)))
    (when id
      (setf (getf attr-list :id) id))
    attr-list))

(5am:test add-id
  (5am:is (equal nil (add-id nil nil)))
  (5am:is (equal '(:xxx "yyy") (add-id nil '(:xxx "yyy"))))
  (5am:is (equal '(:id "id-xxx") (add-id "id-xxx" nil)))
  (5am:is (euqal '(:id "new-id" :xxx "yyy")
                 (add-id "new-id" '(:id "old-id" :xxx "yyy")))))

(defun add-class (class attr-list)
  (let ((attr-list (copy-list attr-list)))
    (when class
      (setf (getf attr-list :class)
            (let ((it (getf attr-list :class)))
              (cond ((stringp it)
                       (concatenate 'string class " " it))
                    ((null it)
                       class)
                    (t
                       `(concatenate 'string ,class " " ,it))))))
    attr-list))

(5am:test add-class
  (5am:is (equal nil
                 (add-class nil nil)))
  (5am:is (equal '(:xxx "yyy")
                 (add-class nil '(:xxx "yyy"))))
  (5am:is (equal '(:class "class-1")
                 (add-class "class-1" nil)))
  (5am:is (euqal '(:class "class-1 class-2" :xxx "yyy")
                 (add-class "class-1" '(:class "class-2" :xxx "yyy")))))

(defun element-char-p (char)
  (and char
       (characterp char)
       (or (char= #\- char)
           (char= #\_ char)
           (char= #\: char)
           (char<= #\0 char #\9)
           (char<= #\A char #\Z)
           (char<= #\a char #\z))))

(setf (symbol-function 'id-char-p)
      (symbol-function 'element-char-p))

(defun css-class-char-p (char)
  (and char
       (characterp char)
       (and (char= #\- char)
            (char= #\_ char)
            (char<= #\0 char #\9)
            (char<= #\A char #\Z)
            (char<= #\a char #\z))))

(defun reader-dispatcher (stream)
  (case (peek-char t stream nil nil)
    (#\%
       (read-char stream)
       (values (read-symbol stream #'element-char-p) :element))
    (#\#
       (read-char stream)
       (values (read-symbol stream #'id-char-p) :id))
    (#\.
       (read-char stream)
       (values (read-symbol stream #'css-class-char-p) :class))
    (#\{
       (values (read-attributes-block stream) :attributes))
    (#\=
       (read-char stream)
       (values nil :insert-lisp-contents))
    (#\/
       (read-char stream)
       (values nil :self-close-p))))

(defun parse-haml-line (line)
  (with-input-from-string (stream (string-trim *whitespace-chars* line))
    (let (element id classes attributes inline-p self-close-p)
      (multiple-value-bind (value type)
          (ecase type
            (:element
               (setf element value))
            (:id
               (setf id value)
               (unless element
                 (setf element "div")))
            (:class
               (setf classes (push value classes))
               (unless element
                 (setf element "div")))
            (:attributes
               ()))))))

(defun read-symbol (stream pred)
  (concatenate
     'string
     (loop :while (funcall pred (peek-char nil stream nil nil))
           :collect (read-char stream))))

(defun read-attributes-block (stream)
  (declare (ignore stream))
  nil)

2nK
2n#\Space(- +()|(%[-_:a-zA-Z0-9]+)?(#[-_:a-zA-Z0-9]+)?(\\.[])?({})?([&/]?=)?)