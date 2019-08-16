(in-package :cl-naive-store)

(defun get-store-from-def (universe store-name)
  (let ((filename (cl-fad:merge-pathnames-as-file
		   (pathname (location universe))
		   (make-pathname :directory (list :relative store-name)
				  :name store-name
				  :type "store")))
	(store-def)
	(store))

    (with-open-file (in filename :if-does-not-exist :create)
      (with-standard-io-syntax              
	(when in
	  (setf store-def (read in nil))
	  (close in))))
    
    (when store-def
      (setf store
	    (make-instance (store-class universe)
			   :name (getf store-def :name)		    
			   :location (getf store-def :location))))
    store))


(defgeneric get-collection-from-def (store collection-name)
  (:documentation "Find the collection definition file and load it."))

(defmethod get-collection-from-def ((store store) collection-name)
  (let ((filename (cl-fad:merge-pathnames-as-file
		   (pathname (location store))
		   (make-pathname :name collection-name
				  :type "col")))
	(collection-def))

    (with-open-file (in filename :if-does-not-exist :create)
      (with-standard-io-syntax
	(when in
	  (setf collection-def (read in nil))
	  (close in))))

    (when collection-def
      (make-instance (collection-class store)
			 :store store
			 :name (getf collection-def :name)
			 :location (getf collection-def :location)))))

(defgeneric naive-reduce (collection function query &key initial-value &allow-other-keys)
  (:documentation "Uses query to select data objects from a collection and then applies the function to 
those objects."))

(defmethod naive-reduce ((collection collection) function query &key initial-value)
    ;;Load if not loaded
    (when (or
	   (not (data-objects collection))
	   (not (loaded-p collection)))
      (load-data collection))

    (reverse (reduce #'(lambda (result object)
			  (if (apply query (list object))
			    (if function		       
				(funcall function result object)
				(push object result))
			    result))
		      (data-objects collection)
		      :initial-value initial-value)))

(defgeneric query-data (collection &key query &allow-other-keys)
  (:documentation "Returns the data that satisfies the query"))

(defmethod query-data ((collection collection) &key query &allow-other-keys)  
  (if query
	(naive-reduce collection nil query :initial-value '())
	(data-objects collection)))

(defmethod query-data ((store store) &key collection-name query &allow-other-keys)
  (let ((collection (get-collection store collection-name)))
    
    (unless collection
      (setf collection (get-collection-from-def 
			store
			collection-name))
      (when collection	
	(add-collection store collection))
      (unless collection
	(error "Could not create collection ~A" collection-name)))
    
    (if query
	(naive-reduce collection nil query :initial-value '())
	(data-objects collection))))

(defgeneric query-data-object (collection &key query &allow-other-keys)
  (:documentation "Returns the first last-data object found, and any others that satisfies the query"))

(defmethod query-data-object ((collection collection) &key query &allow-other-keys)
  (let ((objects (query-data collection :query query)))
    (values (first objects) (rest objects))))

(defmethod query-data-object ((store store) &key collection-name query &allow-other-keys)
  (let ((objects (query-data store :collection-name collection-name :query query)))
    (values (first objects) (rest objects))))

(defmethod query-data ((list list) &key query &allow-other-keys)
  (if query
      (reduce #'(lambda (result object)
		  (when (apply query (list object))		
		    (push result object)))
	      list
	      :initial-value '())
      list))

(defmethod query-data-object ((list list) &key query &allow-other-keys)
   (let ((objects (query-data list :query query)))
    (values (first objects) (rest objects))))

(defmethod query-data-object ((hashtable hash-table) &key query &allow-other-keys)
  (let ((objects))
    (maphash
     (lambda (key object)
       (declare (ignore key))
       (when (funcall query object)
	 (push object objects)))
     hashtable)
    (values (first objects) (rest objects))))
