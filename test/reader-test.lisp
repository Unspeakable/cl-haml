(defpackage :cl-haml-test
  (:use :cl :cl-test-more))
(in-package :cl-haml-test)

(plan 59)

(progn
  (is (cl-haml::blank-string->nil "a") "a")
  (is (cl-haml::blank-string->nil " ") nil)
  (is (cl-haml::blank-string->nil "") nil)
  (is (cl-haml::blank-string->nil nil) nil))

(progn
  (is nil     (cl-haml::get-tag nil nil nil))
  (is nil     (cl-haml::get-tag nil "" nil))
  (is nil     (cl-haml::get-tag nil nil ""))
  (is nil     (cl-haml::get-tag nil "" ""))
  (is :|div|  (cl-haml::get-tag nil "id" nil))
  (is :|div|  (cl-haml::get-tag nil nil "class"))
  (is :|div|  (cl-haml::get-tag nil "id" "class"))
  (is :|div|  (cl-haml::get-tag "%" nil nil))
  (is :|div|  (cl-haml::get-tag "%" "id" nil))
  (is :|div|  (cl-haml::get-tag "%" nil "class"))
  (is :|div|  (cl-haml::get-tag "%" "id" "class"))
  (is :|body| (cl-haml::get-tag "%body" nil nil))
  (is :|body| (cl-haml::get-tag "%body" "id" nil))
  (is :|body| (cl-haml::get-tag "%body" nil "class"))
  (is :|body| (cl-haml::get-tag "%body" "id" "class"))
  (is :|Body| (cl-haml::get-tag "%Body" nil nil))
  (is :body   (cl-haml::get-tag "%BODY" nil nil)))

(progn
  (is nil (cl-haml::get-attr nil))
  (is nil (cl-haml::get-attr ""))
  (is nil (cl-haml::get-attr "a"))
  (is nil (cl-haml::get-attr "az"))
  (is '(:|key| "value") (cl-haml::get-attr "{:key \"value\"}") :test #'equal)
  (is '(:key "value") (cl-haml::get-attr "{:KEY \"value\"}") :test #'equal)
  (is '(:|key| "value" :|key2| "value2")
      (cl-haml::get-attr "{:key \"value\" :key2 \"value2\"}") :test #'equal))

(is nil    (cl-haml::get-id nil))
(is nil    (cl-haml::get-id ""))
(is ""     (cl-haml::get-id "#") :test #'equal)
(is "main" (cl-haml::get-id "#main") :test #'equal)

(progn
  (is "" (cl-haml::get-classes nil nil) :test #'equal)
  (is "" (cl-haml::get-classes "" nil) :test #'equal)
  (is "" (cl-haml::get-classes nil '(:|class| "")) :test #'equal)
  (is "" (cl-haml::get-classes nil '(:|other| "xxx")) :test #'equal)

  (is "xxx" (cl-haml::get-classes ".xxx" nil) :test #'equal)
  (is "xxx yyy" (cl-haml::get-classes ".xxx.yyy" nil) :test #'equal)

  (is "zzz" (cl-haml::get-classes nil '(:|class| "zzz")) :test #'equal)
  (is "zzz xxx" (cl-haml::get-classes ".xxx" '(:|class| "zzz")) :test #'equal)
  (is "zzz xxx yyy" (cl-haml::get-classes ".xxx.yyy" '(:|class| "zzz")) :test #'equal)

  (is '(concatenate 'string (if t "aaa" "bbb") nil)
      (cl-haml::get-classes nil '(:|class| (if t "aaa" "bbb"))) :test #'equal)
  (is ""
      (cl-haml::get-classes nil '(:|class| nil)) :test #'equal)
  (is '(concatenate 'string (if t "aaa" "bbb") " xxx")
      (cl-haml::get-classes ".xxx" '(:|class| (if t "aaa" "bbb"))) :test #'equal))


;; (test get-body
;;   )

;; (test read-concat
;;   )

;; (test gen-standard-result
;;   )

;; (test split-line
;;   )

;; (test make-result
;;   )

;; (test make-filter-result
;;   )

;; (test indent-level
;;   )

;; (test indent-diff
;;   )

;; (test get-id
;;   )

;; (test tag-p
;;   )

;; (test set-tag
;;   )

;; (test get-id
;;   )

;; (test set-text
;;   )

;; (test get-id
;;   )

(ok (not (cl-haml::start= "!" nil)))
(ok (not (cl-haml::start= nil "!")))
(ok (not (cl-haml::start= nil nil)))
(ok (cl-haml::start= "!!!" "!!! rest"))
(ok (cl-haml::start= "" "!!!"))
(ok (not (cl-haml::start= "!!!" "!")))
(ok (cl-haml::start= "!!!" "!!!"))

(progn
  (ok (cl-haml::skip-p ""))
  (ok (cl-haml::skip-p "!!!"))
  (ok (cl-haml::skip-p "!!! XML"))
  (ok (cl-haml::skip-p "-# comment"))
  (ok (cl-haml::skip-p "  -# comment"))
  (ok (not (cl-haml::skip-p "%html")))
  (ok (not (cl-haml::skip-p "  %head")))
  (ok (not (cl-haml::skip-p "    :javascript"))))

;; (test parse
;;   )

;; (test html->sexp
;;   )

(finalize)