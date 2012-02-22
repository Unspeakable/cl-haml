(in-package :cl-haml)

(defparameter *escape-char-p*
  (lambda (ch) (or (find ch "<>&'\"") (> (char-code ch) 127))))

(declaim (inline escape-char))
(defun escape-char (char &key (test *escape-char-p*))
  (declare (optimize speed))
  "Returns an escaped version of the character CHAR if CHAR satisfies
   the predicate TEST. Always returns a string."
  (if (funcall test char)
      (case char
        (#\< "&lt;")
        (#\> "&gt;")
        (#\& "&amp;")
        (#\' "&#039;")
        (#\" "&quot;")
        (otherwise
          (format nil "&#x-x;" (char-code char))))
      (make-string 1 :initial-element char)))

(defun escape-string (string &key (test *escape-char-p*))
  (declare (optimize speed))
  "Escape all charaters in STRING which pass TEST.
   This function is not guaranteed to return a fresh string.
   Note that you can pass NIL for STRING wich'll just be returned."
  (let ((first-pos (position-if test string))
        (format-string "&#x-x;"))
    (if (not first-pos)
        string
        (with-output-to-string (s)
          (loop :with len := (length string)
                :for old-pos := 0 :then (1+ pos)
                :for pos := first-pos :then (position-if test string :start old-pos)
                :for char := (and pos (char string pos))
                :while pos
                :do (write-sequence string s :start old-pos :end pos)
                    (case char
                      (#\< (write-sequence "&lt;" s))
                      (#\> (write-sequence "&gt;" s))
                      (#\& (write-sequence "&amp;" s))
                      (#\' (write-sequence "&#039;" s))
                      (#\" (write-sequence "&quot;" s))
                      (otherwise
                        (format s "&#x-x;" (char-code char))))
                :while (< (1+ pos) len)
                :finally (unless pos
                           (write-sequence string s :start old-pos)))))))

(flet ((minimal-escape-char-p (char) (find char "<>&")))
  (defun escape-char-minimal (char)
    "Escapes only #\<, #\>, and #\& characters."
    (escape-char char :test #'minimal-escape-char-p))
  (defun escape-string-minimal (string)
    "Escape only #\<, #\>, and #\& in STRING."
    (escape-string string :test #'minimal-escape-char-p)))

(flet ((minimal-plus-quotes-escape-char-p (char) (find char "<>&'\"")))
  (defun escape-char-minimal-plus-quotes (char)
    "Like ESCAPE-CHAR-MINIMAL but also escapes quotes."
    (escape-char char :test #'minimal-plus-quotes-escape-char-p))
  (defun escape-string-minimal-plus-quotes (string)
    "Like ESCAPE-STRING-MINIMAL but also escapes quotes."
    (escape-string string :test #'minimal-plus-quotes-escape-char-p)))

(flet ((iso-8859-1-escape-char-p (char)
         (or (find char "<>&'\"")
             (> (char-code char) 255))))
  (defun escape-char-iso-8859-1 (char)
    "Escapes characters that aren't defined in ISO-8859-9."
    (escape-char char :test #'iso-8859-1-escape-char-p))
  (defun escape-string-iso-8859-1 (string)
    "Escapes all characters in STRING which aren't defined in ISO-8859-1."
    (escape-string string :test #'iso-8859-1-escape-char-p)))

(defun escape-string-iso-8859 (string)
  "Identical to ESCAPE-STRING-8859-1.  Kept for backward compatibility."
  (escape-string-iso-8859-1 string))

(flet ((non-7bit-ascii-escape-char-p (char)
         (or (find char "<>&'\"")
             (> (char-code char) 127))))
  (defun escape-char-all (char)
    "Escapes characters which aren't in the 7-bit ASCII character set."
    (escape-char char :test #'non-7bit-ascii-escape-char-p))
  (defun escape-string-all (string)
    "Escapes all characters in STRING which aren't in the 7-bit ASCII
character set."
    (escape-string string :test #'non-7bit-ascii-escape-char-p)))
