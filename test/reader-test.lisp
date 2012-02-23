;(cl:in-package :cl-haml-test)
(cl:in-package :cl-haml)

(setf *function-package* :cl-haml)

(cl-test-more:deftest haml-reader-dispatch
  (macrolet ((is (input-string type result)
               (let ((var (gensym)))
                 `(with-input-from-string (,var ,input-string)
                    (cl-test-more:is
                       (let ((*package* (find-package *function-package*)))
                         (haml-reader-dispatch ,type ,var))
                       ,result
                       :test #'equal)))))
    (is "%html" +haml+ '(+haml+ (:|html|)))
    (is "%head#id1.class1" +haml+ '(+haml+ (:|head| :|class| "class1" :|id| "id1")))
    (is "%meta{:charset \"utf-8\"}" +haml+ '(+haml+ (:|meta| :|charset| "utf-8")))
    (is "%meta{:http-equiv \"content-type\" :content \"text/html\"}" +haml+
        '(+haml+ (:|meta| :|http-equiv| "content-type" :|content| "text/html")))
    (is "%title Hello, world" +haml+ '(+haml+ (:|title| "Hello, world")))
    (is "%body.class1.class2{:class \"class3\"}" +haml+
        '(+haml+ (:|body| :|class| (.class "class1 class2" "class3"))))
    (is "%h1#id2{:id \"id-x\"}" +haml+ '(+haml+ (:|h1| :|id| (.id "id2" "id-x"))))
    (is "%p{:id \"id-x\"}= (format nil \"Hello, World\")" +haml+
        '(+haml+ (:|p| :|id| "id-x"
                  (cl-who:str (format nil "Hello, World")))))
    (is "- dotimes (i 10)" +haml+ '(+lisp+ (dotimes (i 10))))
    (is "= (random 10)" +haml+ '(+haml+ (cl-who:str (random 10))))))

(cl-test-more:run-test-all)
(cl-test-more:finalize)