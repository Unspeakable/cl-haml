(in-package :cl-haml)

(flet ((%read-haml-element-id-classes (input-stream first-char special-char)
         (let ((c (peek-char nil input-stream nil nil)))
           (when (and c (char= c first-char))
             (read-char input-stream)
             (with-output-to-string (out)
               (loop for c = (peek-char nil input-stream nil nil)
                     while (and c
                                (or (char= c  special-char)
                                    (|-_a-zA-Z0-9p| c)))
                     do (write-char (read-char input-stream) out)))))))
  (defun read-tag (stream)
    "Read Haml Tag. ex) %xxx -> :|xxx|."
    (intern (or (%read-haml-element-id-classes stream
                                               #\%
                                               #\:)
                "div")
            :keyword))
  (defun read-id (stream)
    "Read Haml Id. ex) #identification -> \"identification\"."
    (%read-haml-element-id-classes stream
                                   #\#
                                   #\:))
  (defun read-class (stream)
    "Read Haml Class. ex) .class1.class2 -> \"class1 class2\"."
    (substitute #\Space #\.
                (%read-haml-element-id-classes stream
                                               #\.
                                               #\.))))


(defun read-attribute-key (stream &optional (eof-error-p nil)
                                            (eof-value +eof+))
  (if (peek-char t stream eof-error-p eof-value)
      (intern (with-output-to-string (out)
                (read-char stream)
                (loop for char = (peek-char nil stream eof-error-p eof-value)
                      while (or (|-_a-zA-Z0-9p| char)
                                (char= char #\:))
                      do (write-char (read-char stream) out)))
              :keyword)
      (error "Type Error: Attribute key. [~S]" (read stream))))


(defun read-attributes (stream &optional (eof-error-p nil)
                                         (eof-value +eof+))
  (let ((char #1=(peek-char nil stream eof-error-p eof-value)))
    (when (and (characterp char)
               (or (char= char #\()
                   (char= char #\{)))
      ;; Skip '(' or '{'
      (read-char stream)
      (let ((close-paren (if (char= char #\() #\) #\})))
        (loop :for next-char := #1#
              :until (or (eq next-char eof-value)
                         (char= next-char close-paren))
              :collect (read-attribute-key stream) :into result
              :collect (read stream) :into result
              :finally (when (equal next-char close-paren)
                         (read-char stream))
                       (return result))))))

(defun read-options (stream &optional (eof-error-p nil)
                                      (eof-value +eof+))
  "Read options.
But Ignore `self-closing', `inside-whitespace-remove' and
 `outside-whitespace-remove' option."
  (flet ((read-option (stream)
           (let ((c (read-char stream eof-error-p eof-value)))
             (if (and (not (eq eof-value c)) (char/= c #\Space))
                 (case c
                   (#\= (if *escape-html*
                            :escape-insert
                            :insert))
                   (#\& (when (char= (peek-char nil stream) #\=)
                          (read-char stream)
                          :escape-insert))
                   (#\! (when (char= (peek-char nil stream) #\=)
                          (read-char stream)
                          (if *escape-html*
                              :insert
                              :escape-insert)))
                   (#\/ :self-closing)
                   (#\> :inside-whitespace-remove)
                   (#\< :outside-whitespace-remove)
                   (#\Newline (unread-char #\Newline stream)
                              nil))))))
    (loop :for x := (read-option stream)
          :while x
          :collect x)))


(defun read-lisp-content (in)
  (read in))

(defun read-content (stream &optional (eof-error-p nil) (eof-value +eof+))
  (let ((str (with-output-to-string (out)
               (loop :for c := (read-char stream eof-error-p eof-value)
                     :while (and (not (eq eof-value c))
                                 (char/= c #\Newline))
                     :do (unless (char= #\Return c)
                           (write-char c out))))))
    (unless (zerop (length str))
      str)))

(defun read-contents (stream options &optional (eof-error-p nil)
                                               (eof-value +eof+))
  (cond ((member :insert options)
         (let ((content (read-lisp-content stream)))
           `(str ,content)))
        ((member :escape-insert options)
         (let ((content (read-lisp-content stream)))
           (if (stringp content)
               `(esc ,content)
               `(esc (princ-to-string ,content)))))
        (t
         (read-content stream eof-error-p eof-value))))

(defun add-id (id attr-list)
  (let* ((attr-list (copy-list attr-list))
         (target-id (getf attr-list :|id|)))
    (when id
      (setf (getf attr-list :|id|)
            (if target-id
                (if (and (listp target-id)
                         (eq '.id (car target-id)))
                    `(.id ,id ,@(cdr target-id))
                    `(.id ,id ,target-id))
                id)))
    attr-list))

(defun add-class (class attr-list)
  (let ((attr-list (copy-list attr-list)))
    (when class
      (setf (getf attr-list :|class|)
            (let ((it (getf attr-list :|class|)))
              (cond ((null it) class)
                    ((and (listp it) (eq '.class (car it)))
                       `(.class ,class ,@(cdr it)))
                    (t
                       `(.class ,class ,it))))))
    attr-list))

(defun read-haml-element-line (stream &optional (eof-error-p nil)
                                                (eof-value +eof+))
  (let* ((tag   (read-tag stream))
         (id    (read-id  stream))
         (class (read-class stream))
         (attr  (read-attributes stream eof-error-p eof-value))
         (opts  (read-options stream eof-error-p eof-value))
         (body  (read-contents stream opts eof-error-p eof-value)))
    (list +haml+
          `(,@(cons tag (add-class class (add-id id attr)))
            ,@(when body (list body))))))
