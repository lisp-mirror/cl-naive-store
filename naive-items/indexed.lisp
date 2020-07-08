(in-package :cl-naive-items)

(defmethod index-values ((collection item-collection) (values item) &key &allow-other-keys)
  (let ((index-values))
    (dolist (index (indexes collection))
      (push
       (loop for (a b) on (item-values values) by #'cddr
	  when (find a index :test 'equalp)
	  :collect (list a b))
       index-values))
    index-values))
