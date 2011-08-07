(in-package :cl-haml)

(defvar *line-number* 0)

(defvar *in-filter* nil)

(defvar *tag-stack* nil)

(defconstant +eof+ 'eof)

(defconstant* *html5-tags*
  '("a" "b" "table" "tbody" "td" "textarea" "tfoot" "th" "thead" "time" "title"
    "tr" "base" "ul" "var" "video" "bb" "bdo" "blockquote" "body" "br" "button"
    "abbr" "canvas" "caption" "command" "cite" "code" "col" "colgroup"
    "datagrid" "datalist" "del" "details" "dialog" "div" "dfn" "dl" "dt" "dd"
    "address" "em" "embed" "fieldset" "figure" "footer" "form" "h1" "h2" "h3"
    "h4" "h5" "h6" "head" "header" "hr" "html" "i" "iframe" "img" "input" "ins"
    "area" "kbd" "keygen" "label" "legend" "li" "link" "merk" "map" "menu"
    "meta" "article" "meter" "nav" "noscript" "object" "ol" "optgroup" "option"
    "output" "p" "aside" "param" "pre" "progress" "q" "ruby" "rp" "rt" "samp"
    "script" "audio" "section" "section" "select" "small" "source" "span"
    "strong" "style" "sub" "sup"))

(defconstant* +filter+ ":[a-z]*")
(defconstant* +lisp-block+ "- .*")
(defconstant* +tag+ "%[a-zA-Z0-9]*")
(defconstant* +id+ "#[-_a-zA-Z0-9]+")
(defconstant* +classes+ "\\.[-\\._a-zA-Z0-9]+")
(defconstant* +attr+ "{[^}]*}")
(defconstant* +opt+ "/?=")
(defconstant* +body+ ".*")
