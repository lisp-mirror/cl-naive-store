(in-package :cl-naive-store)

(defun plist-to-values (values)
  (loop for (a b) on values by #'cddr 
     :collect b))

(defun plist-to-value-pairs (values)
  (loop for (a b) on values by #'cddr 
     :collect (list a b)))
