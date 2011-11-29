(in-package :cl-who)

(defmethod convert-tag-to-string-list (tag attr-list body body-fn)
  "The standard method which is not specialized. The idea is that you
can use EQL specializers on the forst argument."
  (declare (optimize speed space))
  (let ((tag (string tag)))
    (nconc (when *indent* (list +newline+ (n-spaces *indent*)))
           (list "<" tag)
           (convert-attributes attr-list)
           (let ((list-car (car body))
                 (body-size-1-p (= 1 (length body))))
             (cond ((or (and list-car body-size-1-p)
                        (and (string-equal tag "script")
                             body-size-1-p
                             (null list-car)))
                      (append (list ">")
                              (let ((*indent* nil))
                                (funcall body-fn body))
                              (list "</" tag ">")))
                   ((and (not list-car) body-size-1-p)
                      (list *empty-tag-end*))
                   (t
                      (append (list ">")
                              (let ((*indent* (+ 2 *indent*)))
                                (funcall body-fn body))
                              (list +newline+ (n-spaces *indent*))
                              (list "</" tag ">"))))))))
