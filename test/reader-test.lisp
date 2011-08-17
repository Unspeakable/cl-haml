(defpackage :cl-haml-test
  (:use :cl :cl-test-more))
(in-package :cl-haml-test)

(plan 39)

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

;; (test get-classes
;;   )

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

;; (test parse
;;   )

;; (test html->sexp
;;   )

(finalize)