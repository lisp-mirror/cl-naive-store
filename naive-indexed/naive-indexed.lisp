(in-package :cl-naive-indexed)

;;TODO: Doing partial-indexing doubles the time it takes to load a database
;;Try to delay or spool of partial indexing on different thread.
(defparameter *do-partial-indexing* t)

(defclass indexed-collection-mixin ()
  ((data-objects :initarg :data-objects
	  :accessor data-objects
	  :initform (make-hash-table :test 'equalp)
	  :documentation "Hash table keyed on object hash codes for quick retrieval of an object.")
   (key-value-index :initarg :key-value-index
	  :accessor key-value-index
	  :initform nil
	  :documentation "Hash table keyed on object key values for quick retrieval of an object.
 Used when doing key value equality comparisons.")
   (indexes :initarg :indexes
	    :accessor indexes
	    :initform nil
	    :documentation "List of index combinations. Also indexes members partially if *partial-indexing* is t, for example '((:emp-no :surname gender)) is indexed as (:emp-no :surname :gender), (:emp-no :surname), :emp-no, :surname and :gender"))
  
  (:documentation "Collection extention to add very basic indexes."))

(defclass indexed-values-hashtables-mixin ()
  ()
 (:documentation "Collection extention to use hash tables within the key-value-index hashtable to store/group objects that match a key value combination. DONT use this with big databases with many collections and many objects per collection in SBCL, see NOTE.
") )


(defgeneric hash (object)
  (:documentation "Returns the hash identifier for a data object. Data objects need a hash identifier to work with naive-store-indexed. naive-store-indexed will edit the object to add a hash identifier when adding objects to a collection. naive-store-indexed uses a UUID in its default implementation."))

(defmethod hash (object)
  (frmt "~A" (getx object :hash)))

(defgeneric (setf hash) (value object))

(defmethod (setf hash) (value object)
  (setf (getx object :hash) (frmt "~A" value)))


(defgeneric index-values (collection values &key &allow-other-keys)
  (:documentation "Returns a set of index values from the values of a data object."))

;;NOTE: doing the default without indexed-collection-mixin to give other classes
;;higher up an opportunity to handle this.
(defmethod index-values (collection values &key &allow-other-keys)
  (let ((index-values))
    (dolist (index (indexes collection))
      (push
       (loop for (a b) on values by #'cddr
	  when (find a index :test 'equalp)
	  :collect (list a b))
       index-values))
    index-values))

(defgeneric index-lookup-values (collection values &key &allow-other-keys)
  (:documentation "Looks up object in key value hash index.
If you are not using data-types then the order of values matter."))

(defmethod index-lookup-values ((collection collection) values &key &allow-other-keys)   
  (warn "Not implemented!"))


(defun hash-values (hash-table)
  (when hash-table
    (loop for value being the hash-values of hash-table collect value)))


(defmethod cl-murmurhash:murmurhash ((s uuid:uuid) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash::hash-string (frmt "~A" s) seed mix-only))


(defmethod index-lookup-values ((collection indexed-collection-mixin)
				values &key &allow-other-keys)
  (when (key-value-index collection)
    (remove-duplicates
     (gethash (cl-murmurhash:murmurhash values)
	      (key-value-index collection)))))

(defmethod index-lookup-values ((collection indexed-values-hashtables-mixin)
				values &key &allow-other-keys)
  (when (key-value-index collection)
    (hash-values (gethash (cl-murmurhash:murmurhash values)
			  (key-value-index collection)))))

(defun tree-lookup (index-values tree)
  (lookup (cl-murmurhash:murmurhash index-values)
	  tree))



(defgeneric index-lookup-uuid (collection hash)
  (:documentation "Looks up object in UUID hash index."))

(defmethod index-lookup-uuid ((collection indexed-collection-mixin) hash)
  (gethash (frmt "~A" hash)
	   (data-objects collection)))

(defgeneric index-lookup (collection object)
  (:documentation "Looks up object in UUID hash index and if not found or object does
not have a UUID yet it looks in then value index."))

;;NOTE:Doing this because murmurhash is creating duplicates when you go beyond 10 million index values
(defun try-better-value-match (collection list key-values)
  (dolist (object list)
    (when (equalp key-values (key-values collection object))      
      (return object))))

(defun object-exists (collection object &key key-values)
  (or
   (and (hash object) (index-lookup-uuid collection (hash object)))
   (let ((key-values (or key-values (key-values collection object))))
       (try-better-value-match
	 collection
	 (index-lookup-values collection key-values)
	 key-values))))

(defmethod index-lookup ((collection indexed-collection-mixin) object)
  (object-exists collection object))

(defgeneric add-index (collection object &key &allow-other-keys)
  (:documentation "Adds an object to two indexes. The first uses a UUID that will stay with the object for its life time. The UUID is used when persisting the object and is never changed once created. This allows us to change key values without loosing the identify of the original object. 

The second is a key value hash index to be used when looking for duplicate objects during persist. If you are not using data-types the order of the keys in the plist matter. To make sure that you dont muck with the order of values/keys in your plists initialize all the possible value pairs with nil so that way the order is set."))

(defgeneric push-value-index (collection index-values object &key &allow-other-keys)
  (:documentation "Uses lists within the key-value-index hash-table to store/group objects that match a key value combination. 

On updates of objects could end up with duplicate objects returned by the index lookup. The speed more than makes up for the occactional duplicate for now!

TODO: Implement index-lookup-value that strips out duplicates??"))

(defmethod push-value-index (collection index-values object &key &allow-other-keys)
  (unless (key-value-index collection)
    (setf (key-value-index collection) (make-hash-table :test 'equalp)))

  (push object (gethash (cl-murmurhash:murmurhash index-values)
			      (key-value-index collection))))

(defmethod push-value-index ((collection indexed-values-hashtables-mixin)
			     index-values object &key &allow-other-keys)
  
  (unless (key-value-index collection)
    (setf (key-value-index collection) (make-hash-table :test 'equalp)))
  
  (let ((internal-hash (gethash (cl-murmurhash:murmurhash index-values)
				(key-value-index collection))))       
    (unless internal-hash
      (setf internal-hash (make-hash-table :test 'equalp))
      (setf (gethash (cl-murmurhash:murmurhash index-values) 
		     (key-value-index collection))
	    internal-hash))

    (setf (gethash (hash object) internal-hash) object)))

(defun populate-partial-value-index (collection index-values object)
  (let ((compounded)
	  (compounded-count 1))
    (dolist (pair index-values)      
      (push pair compounded)
      (when (> compounded-count 1)
	(push-value-index collection (reverse compounded) object))

      (push-value-index collection pair object)
      
      (incf compounded-count))))

(defun populate-value-index (collection indexes-values object)
  (dolist (index-values indexes-values)
    (if *do-partial-indexing*
	(populate-partial-value-index collection index-values object)
	(push-value-index collection index-values object))))


(defmethod add-index ((collection indexed-collection-mixin) object &key key-values &allow-other-keys)
  (let* ((index-values (index-values collection object))
	 (key-values (or key-values (key-values collection object))))

    ;;Used to do object value comparisons to find index-object
    ;; (setf (gethash (frmt "~S" key-values) (key-value-index collection)) (list object))
    (push-value-index collection key-values object)

    ;;key-values are in effect their own value index and because it could be completely
    ;;different from index-values it is added to value indexes as well.
    (populate-value-index collection (list key-values) object)
    (populate-value-index collection index-values object)))


(defgeneric remove-index (collection object &key &allow-other-keys)
  (:documentation "Removes a data object from the UUID and key value indexes."))


(defgeneric remove-value-index (collection index-values object &key &allow-other-keys)
  (:documentation ""))

(defmethod remove-value-index (collection index-values object &key &allow-other-keys)
  (when (key-value-index collection)
    (remove object (gethash (cl-murmurhash:murmurhash index-values)
			  (key-value-index collection)))))

(defmethod remove-value-index ((collection indexed-values-hashtables-mixin)
			     index-values object &key &allow-other-keys)
  
  (let ((internal-hash (gethash (cl-murmurhash:murmurhash index-values)
				(key-value-index collection))))       
    
    (when internal-hash
      (remhash (hash object) internal-hash))))


(defun remove-partial-value-index (collection index-values object)
  (let ((compounded)
	  (compounded-count 1))
    (dolist (pair index-values)      
      (push pair compounded)
      (when (> compounded-count 1)
	(remove-value-index collection (reverse compounded) object))

      (remove-value-index collection pair object)
      
      (incf compounded-count))))

(defun remove-index-values (collection indexes-values object)
  (dolist (index-values indexes-values)
    (if *do-partial-indexing*
	(remove-partial-value-index collection index-values object)
	(remove-value-index collection index-values object))))

(defmethod remove-index ((collection indexed-collection-mixin) object &key &allow-other-keys)
  (let ((key-values (key-values collection object))
	(indexes-values (index-values collection object)))

    (remove-value-index collection key-values object)
    (remove-index-values collection (list key-values) object)
    (remove-index-values collection indexes-values object)

    (remhash (hash object) (data-objects collection))))


(defmethod remove-data-object ((collection indexed-collection-mixin) object &key &allow-other-keys)
  (remove-index collection object))

(defmethod add-data-object ((collection indexed-collection-mixin) object
			    &key update-index-p &allow-other-keys)
  "If the an object has no hash and an object with the same keys exists in the collection 
the existing object will be replaced with the new one just as it would have been if a 
matching UUID was found in then collection.

When update-index-p is set to t the indexes will be updated. Just remember that if the object
is really new to the collection the indexes will be updated in any case."
  
  (let* ((key-values (if (not (empty-p (hash object)))
			(key-values collection object)))
	(existing-object (object-exists collection object :key-values key-values)))
    
    (cond (existing-object
	     (setf (hash object) (hash existing-object))
	     (when update-index-p
	       (add-index collection object) :key-values key-values))
	    ((not existing-object)
	     (when (empty-p (hash object))
	       (setf (hash object) (uuid:make-v4-uuid)))
	     (add-index collection object :key-values key-values)))
    
    ;;Add object to the collection
    (setf (gethash (hash object) (data-objects collection)) object)))


(defmethod find-object-by-hash (collection hash)
 (index-lookup-uuid 
  collection
  hash))

(defmethod parse-reference-data-object ((parent-collection indexed-collection-mixin) object &key &allow-other-keys)
  (let ((universe (universe (store parent-collection))))
    
    (let* ((collection (load-object-reference-collection universe object))
	   (ref-object (and collection (find-object-by-hash 
					collection
					(digx object :hash)))))           
      (unless ref-object
	(write-to-file
	 (cl-fad:merge-pathnames-as-file
	  (pathname (location (universe (store collection))))
	  (make-pathname :name "error"
			 :type "err"))	 
	 (list "Could not resolve reference  ~S~%" object)))

      ref-object)))

(defmethod parse-top-level-data-object ((collection indexed-collection-mixin) object &key &allow-other-keys)
  (let ((resolved-values )
	(looked-up-object (index-lookup-uuid 
			   collection
			   (dig object :hash)))
	(final-object))

    (dolist (pair (plist-to-value-pairs object))
      (setf resolved-values
	    (append resolved-values
		    (list (first pair)
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

(defun indexed-values (collection index-values)
  (let ((data))
    (dolist (index index-values)
      (let ((objects (index-lookup-values collection index)))
	(when objects
	  (setf data (append objects data)))))
    data))

(defmethod naive-reduce ((collection indexed-collection-mixin) &key index-values query function initial-value)
  "Uses a list of index values (selects all the listed values), if supplied to preselect data-objects.
The query and funcion is applied to the preselected objects or full collection returning the results."
  
  (naive-reduce (or (indexed-values collection index-values)
		    (data-objects collection))
		:query query
		:function function
		:initial-value initial-value))

(defmethod query-data ((collection indexed-collection-mixin) &key index-values query &allow-other-keys)
  (query-data (or (indexed-values collection index-values)
		  (data-objects collection))
	      :query query))

