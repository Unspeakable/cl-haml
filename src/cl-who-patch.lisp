(in-package :cl-who)

(defun indent-p ()
  (and *indent* (not (minusp *indent*))))

(defmethod convert-tag-to-string-list (tag attr-list body body-fn)
  (let ((tag (string tag)))
    (nconc (when (indent-p)
             (list +newline+ (n-spaces *indent*)))
           (list "<" tag)
           (convert-attributes attr-list)
           (case (length body)
             (0 (if (member tag *html-empty-tags* :test #'string-equal)
                  (list " />")
                  (list "></" tag ">")))
             (1 (if (and (listp (car body)) (keywordp (car body)))
                    (append (list ">")
                            (let ((*indent* (and *indent* (+ 2 (abs *indent*)))))
                              (funcall body-fn body))
                            (when (indent-p)
                              (list +newline+ (n-spaces (abs *indent*))))
                            (list "</" tag ">"))
                    (append (list ">")
                            (let ((*indent* (and *indent* (* -1 (abs *indent*)))))
                              (funcall body-fn body))
                            (list "</" tag ">"))))
             (t (append (list ">")
                        (let ((*indent* (and *indent* (+ 2 (abs *indent*)))))
                          (funcall body-fn body))
                        (when (and *indent* (not (minusp *indent*)))
                          (list +newline+ (n-spaces (abs *indent*)))) 
                        (list "</" tag ">")))))))

(defun tree-to-template (tree)
  (loop :for element :in tree
        :nconc (cond ((and (listp element)
                           (keywordp (first element)))
                      ;; normal tag
                      (process-tag element #'tree-to-template))
                     ((listp element)
                      ;; most likely a normal Lisp form - check if we
                      ;; have nested HTM subtrees
                      (list
                       (replace-htm element #'tree-to-template)))
                     (t
                      (if (indent-p)
                          (list +newline+ (n-spaces *indent*) element)
                          (list element))))))
