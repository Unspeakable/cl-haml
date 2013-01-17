;;; Copyright (c) 2003-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :who)

(defun indent-p ()
  (and *indent* (not (minusp *indent*))))

(defun convert-tag-to-string-list (tag attr-list body body-fn)
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
        :if (and (listp element)
                           (keywordp (first element)))
          :nconc (process-tag element #'tree-to-template)
        :else :if (consp element)
          :collect `(let ((*indent* ,(indent-p)))
                      nil
                      ,element)
        :else
          :nconc (if (indent-p)
                     (list +newline+ (n-spaces *indent*) element)
                     (list element))))
