(in-package :cl-naive-items)

(defmethod deleted-p ((object item))
  (item-deleted-p object))

(defmethod (setf deleted-p) (value (object item) &key &allow-other-keys)
  (setf (item-deleted-p object) value)
  object)

(defmethod persist-object ((collection item-collection) item &key allow-key-change-p)
   (if (item-p item) 
      (persist item
	       :collection collection
	       :allow-key-change-p allow-key-change-p)
      (persist (make-item 
		:store (store collection)
		:collection collection
		:data-type (if (stringp (data-type collection))
			       (item-data-type (data-type collection))
			       (name (data-type collection)))		
		:values item)
	       :allow-key-change-p allow-key-change-p)))
