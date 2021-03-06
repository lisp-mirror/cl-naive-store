(in-package :cl-naive-store.naive-documents)

(defmethod indexed-impl:index-values ((collection document-collection) (values document)
				      &key &allow-other-keys)
  (let ((index-values))
    (dolist (index (indexes collection))
      (push
       (loop for (a b) on (document-values values) by #'cddr
	     when (find a index :test 'equalp)
	       :collect (list a b))
       index-values))
    index-values))
