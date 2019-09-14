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
when doing key value equality comparisons.")
   (indexes :initarg :indexes
	    :accessor indexes
	    :initform nil
	    :documentation "List of index combinations."))
  
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



(defgeneric index-values (collection values &key &allow-other-keys)
  (:documentation "Returns a set of index values from the values of a data object."))

;;NOTE: doing the default without indexed-collection-mixin to give other classes
;;higher up to handle this.
(defmethod index-values (collection values &key &allow-other-keys)
  (loop for (a b) on values by #'cddr
     when (find a (indexes collection) :test 'equalp)
     :collect (list a b)))

(defgeneric index-lookup-values (collection values &key &allow-other-keys)
  (:documentation "Looks up object in key value hash index.
If you are not using data-types then the order of values matter."))

(defmethod index-lookup-values ((collection collection) values &key &allow-other-keys) 
 )

(defmethod index-lookup-values ((collection indexed-collection-mixin) values &key &allow-other-keys)  
  (gethash values
	   (key-value-index collection)))

(defgeneric index-lookup-uuid (collection hash)
  (:documentation "Looks up object in UUID hash index."))

(defmethod index-lookup-uuid ((collection indexed-collection-mixin) hash)
  (gethash (frmt "~A" hash)
	   (data-objects collection)))


(defgeneric add-index (collection object &key &allow-other-keys)
  (:documentation "Adds an object to two indexes. The first uses a UUID that will stay with the object for
 its life time. The UUID is used when persisting the object and is never changed once created. This allows us to 
change key values without loosing the identify of the original object. 

The second is a key value hash index to be used when looking for duplicate objects during persist. If you
are not using data-types the order of the keys in the plist matter. To make sure that you dont muck with the
order of values/keys in your plists initialize all the possible value pairs with nil so that way the order 
is set."))


(defun populate-value-index (collection index-values object)
  (let ((compounded)
	(compounded-count 1))
    (dolist (pair index-values)
      ;;Sanity needs to be maintained
      ;;TODO: Need to make this number configurable, can see some one wanting to index
      ;;every value for some obscure reason.
      (if (< compounded-count 4)
	  (progn
	    (push pair compounded)
	    (when (> compounded-count 1)
	      (setf (gethash (reverse compounded) (key-value-index collection))
		    (push object (gethash (reverse compounded) (key-value-index collection)))))
	    
	    (setf (gethash pair (key-value-index collection))
		  (push object (gethash pair (key-value-index collection))))
	  
	    (incf compounded-count))
	  (return)))))

;;TODO: key-values function her causes a majour speed reduction ...x10... some where along the line
;;suspect its because key-values is a method now
(defmethod add-index ((collection indexed-collection-mixin) object &key &allow-other-keys)

  (let* ((key-values (key-values collection object))
	 (indexed-object (or
			  (index-lookup-uuid collection (hash object))
			  (first (index-lookup-values collection key-values))))
	 (index-values (index-values collection object)))

    
    (when indexed-object
	(setf (hash object) (hash indexed-object)))
    
    (if (empty-p (hash object))
	
	(let ((uuid (uuid:make-v4-uuid)))

	;;add the uuid to the object for persistance
	(setf (hash object) uuid)))


    ;;Used to do object value comparisons to find index-object
    (setf (gethash  key-values (key-value-index collection))
	  (list object))
    
    (populate-value-index collection key-values object)
    (populate-value-index collection index-values object)
    
    
    (setf (gethash (hash object) (data-objects collection)) object)))


(defgeneric remove-index (collection object &key &allow-other-keys)
  (:documentation "Removes a data object from the UUID and key value indexes."))

(defun remove-index-values (collection index-values object)
  (let ((compounded)
	(compounded-count 1))
    
    (dolist (pair index-values)
      ;;Sanity needs to be maintained
      ;;TODO: Need to make this number configurable, can see some one wanting to index
      ;;every value for some obscure reason.
      (if (< compounded-count 4)
	  (progn
	    (push pair compounded)
	    (when (> compounded-count 1)
	      (setf (gethash compounded (key-value-index collection))
		    (remove object (gethash compounded (key-value-index collection)))))
	    (setf (gethash pair (key-value-index collection))
		  (remove object (gethash pair (key-value-index collection))))
	    (incf compounded-count))
	  (return)))))

(defmethod remove-index ((collection indexed-collection-mixin) object &key &allow-other-keys)
  (let ((key-values (key-values collection object))
	(index-values (index-values collection object)))

    (remove-index-values collection key-values object)
    (remove-index-values collection index-values object)

    (remhash (hash object) (data-objects collection))))


(defmethod remove-data-object ((collection indexed-collection-mixin) object &key &allow-other-keys)
  (remove-index collection object))

(defmethod add-data-object ((collection indexed-collection-mixin) object &key &allow-other-keys)
  "If the an object with the same keys exists the collection it will be overridden."
  (add-index collection object))


(defmethod parse-reference-data-object ((parent-collection indexed-collection-mixin) object &key &allow-other-keys)
  (let ((universe (universe (store parent-collection))))
    
    (let* ((collection (load-object-reference-collection universe object))
	   (ref-object (and collection (index-lookup-uuid 
					collection
					(dig object :hash)))))           
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
  
  ;;Load if not loaded
  (when (or
	 (not (data-objects collection))
	 (not (loaded-p collection)))

    (load-data collection))
    
  (naive-reduce (or (indexed-values collection index-values)
		    (data-objects collection))
		:query query
		:function function
		:initial-value initial-value))

(defmethod query-data ((collection indexed-collection-mixin) &key index-values query &allow-other-keys)
  (when (or
	   (not (data-objects collection))
	   (not (loaded-p collection)))

      (load-data collection))

  (query-data (or (indexed-values collection index-values)
		  (data-objects collection))
	      :query query))

