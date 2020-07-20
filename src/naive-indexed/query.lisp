(in-package :cl-naive-indexed)

(defun indexed-values (collection index-values)
  (let ((data))
    (dolist (index index-values)
      (let ((documents (index-lookup-values collection index)))
	(when documents
	  (setf data (append documents data)))))
    data))

(defmethod naive-reduce ((collection indexed-collection-mixin)
			 &key index-values query function initial-value)
  "Extends naive-reduce to be able to take advantage of indexing. Reduce is done on values retrieved by the supplier index."
  
  (naive-reduce (or (indexed-values collection index-values)
		    (documents collection))
		:query query
		:function function
		:initial-value initial-value))

(defmethod query-data ((collection indexed-collection-mixin)
		       &key index-values query &allow-other-keys)
  "Extends query-data to be able to take advantage of indexing. Query is done on values retrieved by the supplier index."
  (query-data (or (indexed-values collection index-values)
		  (documents collection))
	      :query query))




