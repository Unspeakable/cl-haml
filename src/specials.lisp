(in-package :cl-haml)

(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defvar *output-indent-p* t)

(defvar *whitespace-chars*
  '(#\Space #\Return #\LineFeed #\Tab #\Page))

(defconstant +filter+ '+filter+)
(defconstant +lisp+   '+lisp+)
(defconstant +haml+   '+haml+)
(defconstant +haml-multiple-comment+ '+haml-multiple-comment+)
(defconstant +haml-line-comment+     '+haml-line-comment+)

(defconstant +eof+ 'eof)

(defvar *function-package* :cl-user)
(defvar *escape-html* nil "")
(defvar *html-mode* :xhtml "")

(defvar *doctypes*
  `((:xhtml . (("!!!" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
               ("!!! Strict" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
               ("!!! XML" . "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
               ("!!! Frameset" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">")
               ("!!! 5" . "<!DOCTYPE html>")
               ("!!! 1.1" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml1.dtd\">")
               ("!!! Basic" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Basic 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml1.dtd\">")
               ("!!! Mobile" . "<!DOCTYPE html PUBLIC \"-//WAPFORUM//DTD XHTML Mobile 1.2//EN\" \"http://www.openmobilealliance.org/tech/DTD/xhtml-mobile12.dtd\">")
               ("!!! RDFa" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML+RDFa 1.0//EN\" \"http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd\">")))
    (:html4 . (("!!!" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
               ("!!! Strict" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">")
               ("!!! Frameset" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">")))
    (:html5 . (("!!!" . "<!DOCTYPE html>"))))
  "DOCTYPE alist. ")
