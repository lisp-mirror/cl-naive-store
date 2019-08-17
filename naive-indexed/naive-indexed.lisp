(in-package :cl-naive-indexed)

(defclass indexed-collection-mixin ()
  ((data-objects :initarg :data-objects
	  :accessor data-objects
	  :initform (make-hash-table :test 'equalp)
	  :documentation "Hash table keyed on object hash codes for quick retrieval of an object.")
   (key-value-index :initarg :key-value-index
	  :accessor key-value-index
	  :initform (make-hash-table :test 'equalp)
	  :documentation "Hash table keyed on object key values for quick retrieval of an object. Used 
when doing key value equality comparisons."))
  
  (:documentation "Collection extention to add very basic indexes."))

(defgeneric hash (object)
  (:documentation "Returns the hash identifier for a data object. Data objects need a hash identifier
to work with naive-store-indexed. naive-store-indexed will edit the object to add a hash identifier when adding 
objects to a collection. naive-store-indexed uses a UUID in its default implementation."))

(defmethod hash (object)
  (frmt "~A" (getx object :hash)))

(defgeneric (setf hash) (value object))

(defmethod (setf hash) (value object)
  (setf (getx object :hash) (frmt "~A" value)))

(defgeneric key-values (collection values &key &allow-other-keys)
  (:documentation "Returns a set of key values from the values of a data object.
Looks for :key or uses first value."))

(defmethod key-values (collection values &key &allow-other-keys)
  (or (getx values :key)
      (and (equalp (first values) :hash)
	   (fourth values))
      (and (equalp (first values) :deleted-p)
	   (nth 5 values))
      (second values)))

(defgeneric key-values-hash (collection values  &key &allow-other-keys)
  (:documentation "Returns a hash based on the set of key values from the values of a data object."))

(defmethod key-values-hash (collection values  &key &allow-other-keys)
  (let ((key-values (key-values collection values)))
    (sxhash key-values)))


(defgeneric index-lookup-values-hash (collection values &key &allow-other-keys)
  (:documentation "Looks up object in key value hash index."))

(defmethod index-lookup-values-hash ((collection indexed-collection-mixin) values &key &allow-other-keys) 
  (let* ((hashx (key-values-hash collection values)))  
    (gethash hashx
	     (key-value-index collection))))

(defgeneric index-lookup-uuid (collection hash)
  (:documentation "Looks up object in UUID hash index."))

(defmethod index-lookup-uuid ((collection indexed-collection-mixin) hash)
  (gethash (frmt "~A" hash)
	   (data-objects collection)))

(defmethod parse-reference-data-object ((parent-collection indexed-collection-mixin) object &key &allow-other-keys)
  (let ((universe (universe (store parent-collection))))
    
    (let* ((collection (load-object-reference-collection universe object))
	   (ref-object (and collection (index-lookup-uuid 
					collection
					(dig object :hash)))))     
      
      (unless ref-object
	(write-to-file  (format nil "~Aerror.err" (location (universe (store collection))))
			(list "Could not resolve reference  ~S" object)))

      ref-object)))

(defmethod parse-top-level-data-object ((collection indexed-collection-mixin) object &key &allow-other-keys)
  (let ((resolved-values )
	(looked-up-object (index-lookup-uuid 
			   collection
			   (dig object :hash)))
	(final-object))

    (dolist (pair (plist-to-value-pairs object))
      (setf resolved-values (append resolved-values (list (first pair)
							  (parse-data-object collection (second pair))))))

    (setf final-object resolved-values)
    
    (cond (looked-up-object
	   (remove-data-object collection looked-up-object)	   
	   (if (parse-object-deleted-p collection object)
	       (setf final-object nil)
	       (add-data-object collection final-object)))
	  ((not looked-up-object)
	   (unless (getx object :deleted-p)
	     (add-data-object collection final-object))))
    final-object))

(defgeneric add-index (collection object &key &allow-other-keys)
  (:documentation "Adds an object to two indexes. The first uses a UUID that will stay with the object for
 its life time. The UUID is used when persisting the object and is never changed once created. This allows us to 
change key values without loosing the identify of the original object. The second is a key value hash index to
 be used when looking for duplicate objects during persist. The objects hash is also set to UUID."))

(defmethod add-index ((collection indexed-collection-mixin) object &key &allow-other-keys)
    (let* ((indexed-object (if (hash object)
			     (index-lookup-uuid collection (hash object))
			     (index-lookup-values-hash collection object)))
	 (hashx (key-values-hash collection object)))
   
    (when (or
	   (not indexed-object)
	   (empty-p (hash object))
	   ;;TODO: This needs to be removed some time
	   ;;it was used to replace sxhash with UUID's
	   ;;when naive was changed to UUID
	   (string-equal (format nil "~A" (hash object)) (format nil "~A" hashx)))
      
      (let ((uuid (uuid:make-v4-uuid)))
	;;add the uuid to the object for persistance
	(setf (hash object) uuid)))    
    
    (setf (gethash hashx (key-value-index collection)) object)    
    (setf (gethash (hash object) (data-objects collection)) object)))


(defgeneric remove-index (collection object &key &allow-other-keys)
  (:documentation "Removes a data object from the UUID and key value indexes."))

(defmethod remove-index ((collection indexed-collection-mixin) object &key &allow-other-keys)
  (remhash (key-values-hash collection object)
	   (key-value-index collection))
  (remhash (hash object) (data-objects collection)))


(defmethod remove-data-object ((collection indexed-collection-mixin) object &key &allow-other-keys)
  (remove-index collection object))

(defmethod add-data-object ((collection indexed-collection-mixin) object &key &allow-other-keys)
  (add-index collection object))

(defmethod naive-reduce ((collection indexed-collection-mixin) function query &key initial-value)
    ;;Load if not loaded
    (when (or
	   (not (data-objects collection))
	   (not (loaded-p collection)))

      (load-data collection))


    (let ((result initial-value))
      (maphash
       (lambda (key object)
	 (declare (ignore key))
	 (when (funcall query object)
	   (if function		       
	       (setf result (funcall function result object))
	       (push object result))))
       (data-objects collection))

      result))
