(in-package :cl-naive-store)

(defgeneric naive-object-deleted-p (collection object &key &allow-other-keys))

(defmethod naive-object-deleted-p ((collection collection) object &key &allow-other-keys)
  (declare (ignore collection))
  (getf object :deleted-p))

(defgeneric naive-object-p (collection object &key &allow-other-keys))

(defmethod naive-object-p ((collection collection) object &key &allow-other-keys)
  (declare (ignore collection))
  (and (listp object)
       (atom (first object))
       (symbolp (first object))
       (> (length object) 1)
       (not (getx object :reference%))))

(defgeneric naive-reference-object-p (collection object &key &allow-other-keys))

(defmethod naive-reference-object-p ((collection collection) object &key &allow-other-keys)
  (declare (ignore collection))
  (and (listp object)
       (atom (first object))
       (symbolp (first object))
       (> (length object) 1)
       (getx object :reference%)))

(defgeneric parse-top-level-data-object (collection object &key &allow-other-keys))

(defmethod parse-top-level-data-object ((collection collection) object &key &allow-other-keys)
  (let ((resolved-values (parse-data-object collection object))
	(looked-up-object  (index-lookup-uuid 
			   collection
			   (dig object :hash)))
	(final-object))

    (setf final-object resolved-values)
    
    (cond (looked-up-object
	   (remove-data-object collection looked-up-object)	   
	   (if (naive-object-deleted-p collection object)
	       (setf final-object nil)))
	  ((not looked-up-object)
	   (unless (getf object :deleted-p)
	     (push final-object (data-objects collection))
	     (add-index collection final-object))))
    final-object))


(defun load-item-reference-collection (universe item-ref)
  "When items are persisted to file any item values that are referencing an item in a different
collection is first sanitized (just enough info to retrieve the item later is stored). When items are
read from a file the references need to be converted to item objects but for that to happen the collection
containing the referenced items need to be loaded into memory first."
  (let* ((store (get-store* universe (getf item-ref :store)))
	 (collection (get-collection* store (getf item-ref :collection))))
    
    ;;Incase the collection exists but has not been loaded try and load it.
    (when (and collection (not (data-objects collection)))
      (load-data collection))
    
    (unless collection
      (add-collection store collection)
      (load-data collection))
    collection))

(defgeneric parse-reference-data-object (parent-collection object &key &allow-other-keys))

(defmethod parse-reference-data-object ((parent-collection collection) object &key &allow-other-keys)
  (let ((universe (universe (store parent-collection))))
    
    (let* ((collection (load-item-reference-collection universe object))
	   (ref-item (and collection (index-lookup-uuid 
				      collection
				      (dig object :hash)))))     
      
      (unless ref-item

	#|

	(break "~A~% ~A~%~A" collection (dig tree :hash) tree)
	(break "~A" (index-lookup-uuid 
		     collection
		     (dig tree :hash)))
	|#
	
	(write-to-file  (format nil "~Aerror.err" (location (universe (store collection))))
			(list "Could not resolve reference  ~S" object)))

      ref-item)))


(defgeneric parse-child-data-object (parent-collection object &key &allow-other-keys))

(defmethod parse-child-data-object ((parent-collection collection) object &key &allow-other-keys)
  (let* ((resolved-values (and object
			       (parse-data-object parent-collection object))))
    resolved-values))


(defgeneric parse-data-object (collection line &key &allow-other-keys))

(defmethod parse-data-object ((collection collection) object  &key top-level-p &allow-other-keys)
  (cond ((null object)
	 nil)
	(top-level-p
	 (parse-top-level-data-object collection object))
	((naive-object-p collection object)
	 (parse-child-data-object collection object))
	((naive-reference-object-p collection object)
	 (parse-reference-data-object collection object))
	((blob-ref-p object)
	 (read-blob (blob-ref-values object)))
	((atom object)
	 object)
        ((consp object)
	 (mapcar (lambda (child)
		   (parse-data-object collection child))
		 object))
        (t object)))
